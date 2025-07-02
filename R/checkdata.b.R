# Enhanced data quality assessment for clinical research
# Provides comprehensive evaluation of data completeness, accuracy, and patterns

checkdataClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "checkdataClass",
    inherit = checkdataBase,
    private = list(
        
        # Generate interpretation for missing data
        .interpretMissing = function(missing_pct) {
            if (missing_pct == 0) {
                return("Excellent - Complete data")
            } else if (missing_pct < 5) {
                return("Good - Minimal missing data")
            } else if (missing_pct < 15) {
                return("Acceptable - Some missing data")
            } else if (missing_pct < 30) {
                return("Concerning - Substantial missing data")
            } else {
                return("Poor - Extensive missing data")
            }
        },
        
        # Assess skewness interpretation
        .interpretSkewness = function(skewness) {
            abs_skew <- abs(skewness)
            if (abs_skew < 0.5) {
                return("Approximately symmetric")
            } else if (abs_skew < 1) {
                return("Moderately skewed")
            } else {
                return("Highly skewed")
            }
        },
        
        # Determine outlier severity
        .outlierSeverity = function(zscore) {
            abs_z <- abs(zscore)
            if (abs_z > 4) {
                return("Extreme")
            } else if (abs_z > 3.5) {
                return("Very High")
            } else {
                return("High")
            }
        },
        
        # Enhanced data validation with comprehensive error checking
        .validateData = function(variable, var_name) {
            validation_results <- list(
                is_valid = TRUE,
                error_messages = character(),
                warnings = character(),
                recommendations = character()
            )
            
            # Check for completely empty variable
            if (length(variable) == 0) {
                validation_results$is_valid <- FALSE
                validation_results$error_messages <- c(validation_results$error_messages,
                    paste("Variable", var_name, "is empty (length = 0)"))
                return(validation_results)
            }
            
            # Check for all missing data
            if (all(is.na(variable))) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Variable", var_name, "contains only missing values"))
                validation_results$recommendations <- c(validation_results$recommendations,
                    "Consider investigating data collection procedures")
            }
            
            # Check for single value (no variability)
            if (length(unique(na.omit(variable))) == 1) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Variable", var_name, "contains only one unique value"))
                validation_results$recommendations <- c(validation_results$recommendations,
                    "Verify if constant value reflects true data structure")
            }
            
            # Check for very small sample size
            complete_n <- sum(!is.na(variable))
            if (complete_n < 10) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Small sample size (n =", complete_n, ") may limit reliability"))
                validation_results$recommendations <- c(validation_results$recommendations,
                    "Consider collecting additional data or interpreting results cautiously")
            }
            
            # Numeric-specific validations
            if (is.numeric(variable)) {
                clean_var <- variable[!is.na(variable)]
                
                # Check for infinite values
                if (any(is.infinite(clean_var))) {
                    validation_results$warnings <- c(validation_results$warnings,
                        "Infinite values detected in numeric data")
                    validation_results$recommendations <- c(validation_results$recommendations,
                        "Review data processing procedures for infinite value generation")
                }
                
                # Check for extreme range (possible data entry errors)
                if (length(clean_var) > 1) {
                    data_range <- max(clean_var) - min(clean_var)
                    mean_val <- mean(clean_var)
                    if (data_range > 1000 * abs(mean_val)) {
                        validation_results$warnings <- c(validation_results$warnings,
                            "Extremely wide data range detected - possible data entry errors")
                        validation_results$recommendations <- c(validation_results$recommendations,
                            "Verify extreme values for data entry accuracy")
                    }
                }
                
                # Check for negative values in contexts where they shouldn't exist
                if (any(clean_var < 0) && var_name %in% c("age", "weight", "height", "time", "duration", "count")) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Negative values detected in", var_name, "which should typically be positive"))
                    validation_results$recommendations <- c(validation_results$recommendations,
                        "Review negative values for biological/clinical plausibility")
                }
            }
            
            # Categorical-specific validations
            if (is.factor(variable) || is.character(variable)) {
                clean_var <- variable[!is.na(variable)]
                
                # Check for high cardinality (may indicate ID variables)
                unique_count <- length(unique(clean_var))
                if (unique_count > 0.8 * length(clean_var)) {
                    validation_results$warnings <- c(validation_results$warnings,
                        "Very high cardinality - variable may be identifier rather than categorical")
                    validation_results$recommendations <- c(validation_results$recommendations,
                        "Verify if variable should be treated as categorical for analysis")
                }
                
                # Check for inconsistent category encoding
                if (is.character(variable)) {
                    # Look for common inconsistencies
                    unique_vals <- unique(clean_var)
                    if (any(grepl("^(male|female)$", unique_vals, ignore.case = TRUE)) &&
                        any(grepl("^(m|f)$", unique_vals, ignore.case = TRUE))) {
                        validation_results$warnings <- c(validation_results$warnings,
                            "Inconsistent category encoding detected (e.g., 'Male' vs 'M')")
                        validation_results$recommendations <- c(validation_results$recommendations,
                            "Standardize category labels before analysis")
                    }
                }
            }
            
            return(validation_results)
        },
        
        # Advanced outlier detection with multiple methods
        .advancedOutlierDetection = function(variable) {
            clean_var <- variable[!is.na(variable)]
            if (length(clean_var) < 3) {
                return(list(outlier_indices = integer(), methods_used = character()))
            }
            
            outlier_results <- list()
            
            # Method 1: Z-score (standard approach)
            z_scores <- scale(clean_var)[,1]
            z_outliers <- which(abs(z_scores) > 3)
            outlier_results$zscore <- list(
                indices = z_outliers,
                values = clean_var[z_outliers],
                scores = z_scores[z_outliers]
            )
            
            # Method 2: IQR method (robust to skewness)
            Q1 <- quantile(clean_var, 0.25)
            Q3 <- quantile(clean_var, 0.75)
            IQR <- Q3 - Q1
            iqr_outliers <- which(clean_var < (Q1 - 1.5 * IQR) | clean_var > (Q3 + 1.5 * IQR))
            outlier_results$iqr <- list(
                indices = iqr_outliers,
                values = clean_var[iqr_outliers],
                bounds = c(Q1 - 1.5 * IQR, Q3 + 1.5 * IQR)
            )
            
            # Method 3: Modified Z-score (MAD-based, robust)
            if (length(clean_var) > 3) {
                mad_val <- mad(clean_var, constant = 1.4826)  # Consistency factor for normal distribution
                if (mad_val > 0) {
                    modified_z <- 0.6745 * (clean_var - median(clean_var)) / mad_val
                    mad_outliers <- which(abs(modified_z) > 3.5)
                    outlier_results$mad <- list(
                        indices = mad_outliers,
                        values = clean_var[mad_outliers],
                        scores = modified_z[mad_outliers]
                    )
                }
            }
            
            # Consensus outliers (detected by multiple methods)
            all_outlier_indices <- unique(c(outlier_results$zscore$indices, 
                                           outlier_results$iqr$indices,
                                           if(!is.null(outlier_results$mad)) outlier_results$mad$indices))
            
            # Count detections per point
            detection_count <- rep(0, length(clean_var))
            detection_count[outlier_results$zscore$indices] <- detection_count[outlier_results$zscore$indices] + 1
            detection_count[outlier_results$iqr$indices] <- detection_count[outlier_results$iqr$indices] + 1
            if (!is.null(outlier_results$mad)) {
                detection_count[outlier_results$mad$indices] <- detection_count[outlier_results$mad$indices] + 1
            }
            
            # Classify outlier confidence
            consensus_outliers <- which(detection_count >= 2)  # Detected by at least 2 methods
            
            return(list(
                outlier_indices = consensus_outliers,
                detection_count = detection_count[consensus_outliers],
                all_methods = outlier_results,
                methods_used = c("Z-score", "IQR", if(!is.null(outlier_results$mad)) "Modified Z-score")
            ))
        },
        
        # Enhanced missing data pattern analysis
        .analyzeMissingPatterns = function(variable, data_context = NULL) {
            missing_indices <- which(is.na(variable))
            complete_indices <- which(!is.na(variable))
            n_total <- length(variable)
            missing_pct <- 100 * length(missing_indices) / n_total
            
            patterns <- list()
            
            # Pattern 1: Completely random missing (MCAR assessment)
            if (length(missing_indices) > 0 && length(complete_indices) > 0) {
                # Runs test for randomness
                if (length(missing_indices) >= 5 && length(complete_indices) >= 5) {
                    missing_binary <- is.na(variable)
                    runs <- rle(missing_binary)
                    n_runs <- length(runs$lengths)
                    expected_runs <- 2 * length(missing_indices) * length(complete_indices) / n_total + 1
                    
                    if (n_runs < expected_runs * 0.5) {
                        patterns$clustering <- "Missing data appears clustered - possible systematic cause"
                    } else if (n_runs > expected_runs * 1.5) {
                        patterns$alternating <- "Missing data alternates frequently - check data collection pattern"
                    } else {
                        patterns$random <- "Missing data pattern appears random"
                    }
                }
            }
            
            # Pattern 2: Monotone missing (dropout pattern)
            if (length(missing_indices) > n_total * 0.1) {
                # Check if missing data concentrates at end (dropout)
                last_quarter_start <- round(n_total * 0.75)
                missing_in_last_quarter <- sum(missing_indices > last_quarter_start)
                if (missing_in_last_quarter > length(missing_indices) * 0.6) {
                    patterns$dropout <- "Missing data concentrates toward end - possible dropout pattern"
                }
            }
            
            # Pattern 3: Missing data percentage thresholds
            if (missing_pct > 50) {
                patterns$severe <- paste("Severe missing data (", round(missing_pct, 1), "%) - major quality concern")
            } else if (missing_pct > 20) {
                patterns$substantial <- paste("Substantial missing data (", round(missing_pct, 1), "%) - investigate causes")
            }
            
            return(patterns)
        },
        
        # Enhanced categorical data analysis
        .analyzeCategoricalQuality = function(variable) {
            if (!is.factor(variable) && !is.character(variable)) {
                return(NULL)
            }
            
            clean_var <- variable[!is.na(variable)]
            if (length(clean_var) == 0) {
                return(list(quality_issues = "All values missing"))
            }
            
            category_analysis <- list()
            
            # Category frequency analysis
            freq_table <- table(clean_var)
            n_categories <- length(freq_table)
            n_total <- length(clean_var)
            
            # Category balance assessment
            if (n_categories > 1) {
                min_freq <- min(freq_table)
                max_freq <- max(freq_table)
                balance_ratio <- min_freq / max_freq
                
                if (balance_ratio < 0.1) {
                    category_analysis$imbalance <- "Severe category imbalance detected"
                } else if (balance_ratio < 0.3) {
                    category_analysis$moderate_imbalance <- "Moderate category imbalance"
                } else {
                    category_analysis$balanced <- "Categories reasonably balanced"
                }
                
                # Rare category detection
                rare_categories <- names(freq_table)[freq_table < 5]
                if (length(rare_categories) > 0) {
                    category_analysis$rare_categories <- paste("Rare categories with <5 observations:",
                                                             paste(rare_categories, collapse = ", "))
                }
            }
            
            # High cardinality check
            cardinality_ratio <- n_categories / n_total
            if (cardinality_ratio > 0.8) {
                category_analysis$high_cardinality <- "Very high cardinality - may be identifier variable"
            } else if (cardinality_ratio > 0.5) {
                category_analysis$moderate_cardinality <- "High cardinality - verify categorical nature"
            }
            
            # Category naming consistency (for character variables)
            if (is.character(variable)) {
                unique_vals <- unique(clean_var)
                
                # Check for case inconsistencies
                lower_vals <- tolower(unique_vals)
                if (length(unique(lower_vals)) < length(unique_vals)) {
                    category_analysis$case_inconsistency <- "Case inconsistencies detected in categories"
                }
                
                # Check for leading/trailing spaces
                trimmed_vals <- trimws(unique_vals)
                if (any(trimmed_vals != unique_vals)) {
                    category_analysis$whitespace_issues <- "Leading/trailing spaces detected in categories"
                }
            }
            
            return(category_analysis)
        },
        
        # Clinical context validation
        .clinicalContextValidation = function(variable, var_name) {
            if (!is.numeric(variable)) {
                return(NULL)
            }
            
            clean_var <- variable[!is.na(variable)]
            if (length(clean_var) == 0) {
                return(NULL)
            }
            
            clinical_issues <- list()
            
            # Age-specific validations
            if (grepl("age", var_name, ignore.case = TRUE)) {
                if (any(clean_var < 0)) {
                    clinical_issues$negative_age <- "Negative age values detected"
                }
                if (any(clean_var > 120)) {
                    clinical_issues$extreme_age <- "Age values >120 years detected - verify accuracy"
                }
                if (any(clean_var < 1 & clean_var > 0)) {
                    clinical_issues$fractional_age <- "Fractional age values <1 detected - verify units"
                }
            }
            
            # Weight-specific validations
            if (grepl("weight", var_name, ignore.case = TRUE)) {
                if (any(clean_var < 1)) {
                    clinical_issues$low_weight <- "Extremely low weight values detected - verify units"
                }
                if (any(clean_var > 300)) {
                    clinical_issues$high_weight <- "Weight values >300 detected - verify accuracy"
                }
            }
            
            # Height-specific validations
            if (grepl("height", var_name, ignore.case = TRUE)) {
                if (any(clean_var > 250 & clean_var < 300)) {
                    clinical_issues$height_units <- "Height values suggest cm units (normal for adults)"
                } else if (any(clean_var < 10 & clean_var > 3)) {
                    clinical_issues$height_feet <- "Height values suggest feet units"
                } else if (any(clean_var < 3)) {
                    clinical_issues$height_meters <- "Height values suggest meter units"
                }
                
                if (any(clean_var < 0.5) || any(clean_var > 300)) {
                    clinical_issues$implausible_height <- "Implausible height values detected"
                }
            }
            
            # Laboratory value ranges (basic checks)
            if (grepl("hemoglobin|hgb|hb", var_name, ignore.case = TRUE)) {
                if (any(clean_var < 3) || any(clean_var > 20)) {
                    clinical_issues$hemoglobin_range <- "Hemoglobin values outside typical range (3-20 g/dL)"
                }
            }
            
            if (grepl("creatinine", var_name, ignore.case = TRUE)) {
                if (any(clean_var < 0.3) || any(clean_var > 10)) {
                    clinical_issues$creatinine_range <- "Creatinine values outside typical range (0.3-10 mg/dL)"
                }
            }
            
            return(clinical_issues)
        },
        
        .run = function() {
            # Enhanced input validation with user guidance
            if (is.null(self$options$var)) {
                todo <- "
                <h3>📊 ClinicoPath Data Quality Assessment</h3>
                <p><strong>Purpose:</strong> Comprehensive evaluation of data completeness, accuracy, and patterns for clinical research.</p>
                
                <h4>📋 Required Input:</h4>
                <ul>
                    <li><strong>Variable to Check:</strong> Select any variable for quality assessment</li>
                </ul>
                
                <h4>⚙️ Analysis Options:</h4>
                <ul>
                    <li><strong>Outlier Analysis:</strong> Statistical outlier detection using multiple methods</li>
                    <li><strong>Distribution Analysis:</strong> Descriptive statistics and normality assessment</li>
                    <li><strong>Duplicate Analysis:</strong> Identify repeated values and patterns</li>
                    <li><strong>Pattern Analysis:</strong> Missing data mechanisms and systematic issues</li>
                </ul>
                
                <h4>🔬 Assessment Dimensions:</h4>
                <ul>
                    <li><strong>Completeness:</strong> Missing data evaluation and impact assessment</li>
                    <li><strong>Accuracy:</strong> Outlier detection and range validation</li>
                    <li><strong>Consistency:</strong> Pattern recognition and systematic issues</li>
                    <li><strong>Clinical Validity:</strong> Context-specific validation (age, lab values, etc.)</li>
                </ul>
                
                <h4>📈 Quality Grading:</h4>
                <ul>
                    <li><strong>Grade A:</strong> Excellent quality - ready for analysis</li>
                    <li><strong>Grade B:</strong> Good quality - minor issues documented</li>
                    <li><strong>Grade C:</strong> Concerning quality - cleaning recommended</li>
                    <li><strong>Grade D:</strong> Poor quality - major intervention required</li>
                </ul>
                
                <h4>🏥 Clinical Applications:</h4>
                <ul>
                    <li><strong>Clinical Trials:</strong> Regulatory compliance and data monitoring</li>
                    <li><strong>Observational Studies:</strong> Data integrity assessment</li>
                    <li><strong>Quality Improvement:</strong> Systematic quality monitoring</li>
                    <li><strong>Publication Preparation:</strong> Data quality documentation</li>
                </ul>
                
                <p><strong>Resources:</strong></p>
                <ul>
                    <li><a href='https://clinicopath.github.io/ClinicoPathJamoviModule/' target='_blank'>User Guide</a></li>
                    <li><a href='https://www.fda.gov/regulatory-information/search-fda-guidance-documents/e6-r2-good-clinical-practice-integrated-addendum-ich-e6r1' target='_blank'>ICH E6 GCP Guidelines</a></li>
                </ul>
                "
                self$results$qualityText$setContent(todo)
                return()
            }
            
            # Enhanced error checking and validation
            if (nrow(self$data) == 0) {
                stop('Error: Dataset contains no rows. Please provide data for quality assessment.')
            }

            # Get variable data with enhanced validation
            variable <- self$data[[self$options$var]]
            var_name <- self$options$var
            
            # Comprehensive data validation
            validation_results <- private$.validateData(variable, var_name)
            
            # Handle validation errors
            if (!validation_results$is_valid) {
                stop(paste("Data Validation Error:", paste(validation_results$error_messages, collapse = "; ")))
            }
            
            # Basic data characteristics with enhanced calculations
            n_total <- length(variable)
            n_missing <- sum(is.na(variable))
            n_complete <- n_total - n_missing
            n_unique <- length(unique(na.omit(variable)))
            missing_pct <- round(100 * n_missing / n_total, 1)
            unique_pct <- ifelse(n_complete > 0, round(100 * n_unique / n_complete, 1), 0)
            
            # Variable type detection with enhanced logic
            is_numeric <- is.numeric(variable)
            is_categorical <- is.factor(variable) || is.character(variable)
            is_logical <- is.logical(variable)
            
            # Enhanced missing value analysis with clinical interpretation
            self$results$missingVals$addRow(rowKey=1, values=list(
                metric="Total Observations",
                value=as.character(n_total),
                interpretation=ifelse(n_total >= 100, "Adequate sample size", 
                                    ifelse(n_total >= 30, "Moderate sample size", "Small sample size"))
            ))
            
            self$results$missingVals$addRow(rowKey=2, values=list(
                metric="Missing Values",
                value=sprintf("%d (%.1f%%)", n_missing, missing_pct),
                interpretation=private$.interpretMissing(missing_pct)
            ))
            
            self$results$missingVals$addRow(rowKey=3, values=list(
                metric="Complete Cases",
                value=sprintf("%d (%.1f%%)", n_complete, 100-missing_pct),
                interpretation=ifelse(n_complete >= 0.9 * n_total, "Excellent completeness", 
                                    ifelse(n_complete >= 0.8 * n_total, "Good completeness", 
                                          ifelse(n_complete >= 0.7 * n_total, "Acceptable completeness", "Poor completeness")))
            ))
            
            self$results$missingVals$addRow(rowKey=4, values=list(
                metric="Unique Values",
                value=sprintf("%d (%.1f%%)", n_unique, unique_pct),
                interpretation=ifelse(unique_pct > 95, "Very high variability", 
                                    ifelse(unique_pct > 50, "High variability",
                                          ifelse(unique_pct > 10, "Moderate variability", "Low variability")))
            ))

            # Advanced outlier detection for numeric variables
            outliers_found <- 0
            if (self$options$showOutliers && is_numeric && n_complete >= 3) {
                outlier_analysis <- private$.advancedOutlierDetection(variable)
                outliers_found <- length(outlier_analysis$outlier_indices)
                
                if (outliers_found > 0) {
                    # Get original row numbers
                    complete_indices <- which(!is.na(variable))
                    
                    for (i in seq_along(outlier_analysis$outlier_indices)) {
                        outlier_idx <- outlier_analysis$outlier_indices[i]
                        original_row <- complete_indices[outlier_idx]
                        clean_var <- variable[!is.na(variable)]
                        
                        # Enhanced severity assessment
                        z_score <- scale(clean_var)[outlier_idx,1]
                        confidence_level <- ifelse(length(outlier_analysis$detection_count) >= i, 
                                                 outlier_analysis$detection_count[i], 1)
                        
                        severity_text <- paste(private$.outlierSeverity(z_score), 
                                             "(", confidence_level, "/", length(outlier_analysis$methods_used), "methods)")
                        
                        self$results$outliers$addRow(rowKey=i, values=list(
                            rowNumber=original_row,
                            value=clean_var[outlier_idx],
                            zscore=round(z_score, 3),
                            severity=severity_text
                        ))
                    }
                }
            }

            # Enhanced distribution analysis for numeric variables
            if (self$options$showDistribution && is_numeric && n_complete >= 2) {
                clean_var <- variable[!is.na(variable)]
                
                mean_val <- mean(clean_var)
                median_val <- median(clean_var)
                sd_val <- sd(clean_var)
                
                # Enhanced skewness calculation with interpretation
                skewness <- ifelse(sd_val > 0 && length(clean_var) > 2, 
                    mean((clean_var - mean_val)^3) / sd_val^3, 
                    0)
                
                # Enhanced coefficient of variation with clinical context
                cv <- ifelse(mean_val != 0, abs(sd_val / mean_val) * 100, 0)
                
                # Kurtosis calculation (measure of tail heaviness)
                kurtosis <- ifelse(sd_val > 0 && length(clean_var) > 3,
                    mean((clean_var - mean_val)^4) / sd_val^4 - 3,
                    0)
                
                self$results$distribution$addRow(rowKey=1, values=list(
                    metric="Mean",
                    value=round(mean_val, 4),
                    interpretation=ifelse(cv < 10, "Stable central value", "Variable central tendency")
                ))
                
                self$results$distribution$addRow(rowKey=2, values=list(
                    metric="Median", 
                    value=round(median_val, 4),
                    interpretation=ifelse(abs(mean_val - median_val) / sd_val < 0.2, 
                                        "Close to mean (symmetric)", 
                                        "Different from mean (skewed)")
                ))
                
                self$results$distribution$addRow(rowKey=3, values=list(
                    metric="Standard Deviation",
                    value=round(sd_val, 4),
                    interpretation=ifelse(cv < 15, "Low variability (good precision)", 
                                        ifelse(cv < 35, "Moderate variability", "High variability (precision concern)"))
                ))
                
                self$results$distribution$addRow(rowKey=4, values=list(
                    metric="Coefficient of Variation (%)",
                    value=round(cv, 2),
                    interpretation=ifelse(cv < 10, "Excellent precision", 
                                        ifelse(cv < 20, "Good precision", 
                                              ifelse(cv < 50, "Moderate precision", "Poor precision")))
                ))
                
                self$results$distribution$addRow(rowKey=5, values=list(
                    metric="Skewness",
                    value=round(skewness, 3),
                    interpretation=private$.interpretSkewness(skewness)
                ))
                
                # Enhanced range analysis with outlier context
                min_val <- min(clean_var)
                max_val <- max(clean_var)
                range_val <- max_val - min_val
                
                self$results$distribution$addRow(rowKey=6, values=list(
                    metric="Range",
                    value=round(range_val, 4),
                    interpretation=sprintf("From %.3f to %.3f", min_val, max_val)
                ))
                
                # Add quartile information
                q1 <- quantile(clean_var, 0.25)
                q3 <- quantile(clean_var, 0.75)
                iqr <- q3 - q1
                
                self$results$distribution$addRow(rowKey=7, values=list(
                    metric="Interquartile Range",
                    value=round(iqr, 4),
                    interpretation=sprintf("Q1: %.3f, Q3: %.3f", q1, q3)
                ))
            }

            # Enhanced duplicate analysis with categorical support
            if (self$options$showDuplicates && n_complete > 0) {
                clean_var <- variable[!is.na(variable)]
                freq_table <- table(clean_var)
                
                # Enhanced duplicate detection
                if (is_categorical) {
                    # For categorical data, show all frequencies
                    freq_table_sorted <- sort(freq_table, decreasing = TRUE)
                    max_display <- min(20, length(freq_table_sorted))  # Limit display
                    
                    for (i in 1:max_display) {
                        dup_pct <- round(100 * freq_table_sorted[i] / n_complete, 1)
                        self$results$duplicates$addRow(rowKey=i, values=list(
                            value=names(freq_table_sorted)[i],
                            count=as.integer(freq_table_sorted[i]),
                            percentage=dup_pct
                        ))
                    }
                } else {
                    # For numeric data, show only duplicates
                    duplicates <- freq_table[freq_table > 1]
                    
                    if (length(duplicates) > 0) {
                        # Sort by frequency (descending)
                        duplicates <- sort(duplicates, decreasing = TRUE)
                        max_display <- min(15, length(duplicates))  # Limit display
                        
                        for (i in 1:max_display) {
                            dup_pct <- round(100 * duplicates[i] / n_complete, 1)
                            self$results$duplicates$addRow(rowKey=i, values=list(
                                value=names(duplicates)[i],
                                count=as.integer(duplicates[i]),
                                percentage=dup_pct
                            ))
                        }
                    }
                }
            }

            # Enhanced data patterns analysis with advanced detection
            if (self$options$showPatterns) {
                pattern_count <- 1
                
                # Advanced missing data pattern analysis
                missing_patterns <- private$.analyzeMissingPatterns(variable)
                for (pattern_name in names(missing_patterns)) {
                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern=paste("Missing Data:", stringr::str_to_title(gsub("_", " ", pattern_name))),
                        description=missing_patterns[[pattern_name]],
                        recommendation=ifelse(missing_pct > 20, "Investigate missing data mechanisms", 
                                            "Document missing data pattern")
                    ))
                    pattern_count <- pattern_count + 1
                }
                
                # Categorical data quality patterns
                if (is_categorical) {
                    categorical_issues <- private$.analyzeCategoricalQuality(variable)
                    if (!is.null(categorical_issues)) {
                        for (issue_name in names(categorical_issues)) {
                            self$results$patterns$addRow(rowKey=pattern_count, values=list(
                                pattern=paste("Categorical:", stringr::str_to_title(gsub("_", " ", issue_name))),
                                description=categorical_issues[[issue_name]],
                                recommendation=ifelse(grepl("imbalance", categorical_issues[[issue_name]]), 
                                                    "Consider sampling strategy or analysis method", 
                                                    "Review category definitions and data entry")
                            ))
                            pattern_count <- pattern_count + 1
                        }
                    }
                }
                
                # Clinical context validation patterns
                clinical_issues <- private$.clinicalContextValidation(variable, var_name)
                if (!is.null(clinical_issues) && length(clinical_issues) > 0) {
                    for (issue_name in names(clinical_issues)) {
                        self$results$patterns$addRow(rowKey=pattern_count, values=list(
                            pattern=paste("Clinical Validation:", stringr::str_to_title(gsub("_", " ", issue_name))),
                            description=clinical_issues[[issue_name]],
                            recommendation="Verify clinical plausibility and measurement units"
                        ))
                        pattern_count <- pattern_count + 1
                    }
                }
                
                # Data validation warnings integration
                if (length(validation_results$warnings) > 0) {
                    for (warning_msg in validation_results$warnings) {
                        self$results$patterns$addRow(rowKey=pattern_count, values=list(
                            pattern="Data Validation Warning",
                            description=warning_msg,
                            recommendation="Review data collection procedures"
                        ))
                        pattern_count <- pattern_count + 1
                    }
                }
                
                # Low uniqueness pattern (enhanced)
                if (n_complete > 0 && (n_unique / n_complete < 0.1)) {
                    uniqueness_interpretation <- ifelse(n_unique == 1, "Constant value detected",
                                                      ifelse(n_unique / n_complete < 0.05, "Very low uniqueness",
                                                            "Low uniqueness"))
                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern="Data Variability",
                        description=sprintf("%s: %.1f%% unique values (%d/%d)", 
                                          uniqueness_interpretation, unique_pct, n_unique, n_complete),
                        recommendation=ifelse(n_unique == 1, "Investigate constant value cause", 
                                            "Verify if low variability reflects true data structure")
                    ))
                    pattern_count <- pattern_count + 1
                }
                
                # High outlier rate pattern (enhanced)
                if (outliers_found > 0.05 * n_complete) {
                    outlier_rate_pct <- round(100 * outliers_found / n_complete, 1)
                    severity_desc <- ifelse(outlier_rate_pct > 15, "Very high", 
                                          ifelse(outlier_rate_pct > 10, "High", "Elevated"))
                    
                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern="Outlier Pattern",
                        description=sprintf("%s outlier rate: %d outliers (%.1f%% of data)", 
                                          severity_desc, outliers_found, outlier_rate_pct),
                        recommendation="Investigate measurement procedures and consider robust analysis methods"
                    ))
                    pattern_count <- pattern_count + 1
                }
            }

            # Enhanced comprehensive quality assessment with clinical grading
            quality_grade <- "A"
            quality_issues <- c()
            quality_score <- 100  # Start with perfect score
            
            # Enhanced quality grading with weighted scoring
            # Missing data assessment (30% of total score)
            missing_penalty <- 0
            if (missing_pct > 50) {
                missing_penalty <- 40
                quality_issues <- c(quality_issues, "severe missing data (>50%)")
            } else if (missing_pct > 30) {
                missing_penalty <- 25
                quality_issues <- c(quality_issues, "extensive missing data (30-50%)")
            } else if (missing_pct > 15) {
                missing_penalty <- 15
                quality_issues <- c(quality_issues, "substantial missing data (15-30%)")
            } else if (missing_pct > 5) {
                missing_penalty <- 5
            }
            quality_score <- quality_score - missing_penalty
            
            # Outlier assessment (25% of total score)
            outlier_rate <- ifelse(n_complete > 0, outliers_found / n_complete, 0)
            outlier_penalty <- 0
            if (outlier_rate > 0.15) {
                outlier_penalty <- 30
                quality_issues <- c(quality_issues, "very high outlier rate (>15%)")
            } else if (outlier_rate > 0.10) {
                outlier_penalty <- 20
                quality_issues <- c(quality_issues, "high outlier rate (10-15%)")
            } else if (outlier_rate > 0.05) {
                outlier_penalty <- 10
                quality_issues <- c(quality_issues, "elevated outlier rate (5-10%)")
            }
            quality_score <- quality_score - outlier_penalty
            
            # Variability assessment (20% of total score)
            variability_penalty <- 0
            if (n_complete > 0) {
                uniqueness_ratio <- n_unique / n_complete
                if (uniqueness_ratio < 0.01) {
                    variability_penalty <- 25
                    quality_issues <- c(quality_issues, "extremely low variability (<1% unique)")
                } else if (uniqueness_ratio < 0.05) {
                    variability_penalty <- 15
                    quality_issues <- c(quality_issues, "very low variability (<5% unique)")
                } else if (uniqueness_ratio < 0.1) {
                    variability_penalty <- 8
                    quality_issues <- c(quality_issues, "low variability (<10% unique)")
                }
            }
            quality_score <- quality_score - variability_penalty
            
            # Clinical validity assessment (15% of total score)
            clinical_penalty <- 0
            clinical_issues <- private$.clinicalContextValidation(variable, var_name)
            if (!is.null(clinical_issues) && length(clinical_issues) > 0) {
                clinical_penalty <- min(20, length(clinical_issues) * 5)
                quality_issues <- c(quality_issues, "clinical validity concerns")
            }
            quality_score <- quality_score - clinical_penalty
            
            # Sample size assessment (10% of total score)
            sample_penalty <- 0
            if (n_total < 10) {
                sample_penalty <- 30
                quality_issues <- c(quality_issues, "very small sample size")
            } else if (n_total < 30) {
                sample_penalty <- 15
                quality_issues <- c(quality_issues, "small sample size")
            } else if (n_total < 50) {
                sample_penalty <- 5
            }
            quality_score <- quality_score - sample_penalty
            
            # Convert score to letter grade
            if (quality_score >= 90) {
                quality_grade <- "A"
            } else if (quality_score >= 80) {
                quality_grade <- "B"
            } else if (quality_score >= 70) {
                quality_grade <- "C"
            } else {
                quality_grade <- "D"
            }
            
            # Enhanced quality summary with clinical research context
            quality_text <- sprintf("═══════════════════════════════════════════════════════\n")
            quality_text <- paste0(quality_text, sprintf("   DATA QUALITY ASSESSMENT FOR '%s'\n", toupper(var_name)))
            quality_text <- paste0(quality_text, sprintf("═══════════════════════════════════════════════════════\n\n"))
            
            quality_text <- paste0(quality_text, sprintf("📊 OVERALL QUALITY GRADE: %s (Score: %d/100)\n\n", quality_grade, round(quality_score)))
            
            # Variable type and basic characteristics
            var_type_desc <- ifelse(is_numeric, "Numeric/Continuous", 
                                   ifelse(is_categorical, "Categorical/Factor", "Other"))
            quality_text <- paste0(quality_text, sprintf("📋 VARIABLE CHARACTERISTICS:\n"))
            quality_text <- paste0(quality_text, sprintf("• Variable Type: %s\n", var_type_desc))
            quality_text <- paste0(quality_text, sprintf("• Total Observations: %d\n", n_total))
            quality_text <- paste0(quality_text, sprintf("• Complete Cases: %d (%.1f%%)\n", n_complete, 100-missing_pct))
            quality_text <- paste0(quality_text, sprintf("• Unique Values: %d (%.1f%% of complete cases)\n\n", n_unique, unique_pct))
            
            # Completeness assessment
            quality_text <- paste0(quality_text, "🔍 COMPLETENESS ASSESSMENT:\n")
            quality_text <- paste0(quality_text, sprintf("• Missing Data Rate: %.1f%% (%d/%d observations)\n", 
                                                        missing_pct, n_missing, n_total))
            quality_text <- paste0(quality_text, sprintf("• Completeness Grade: %s\n", 
                                                        private$.interpretMissing(missing_pct)))
            
            # Add missing pattern information if available
            if (n_missing > 0 && self$options$showPatterns) {
                missing_patterns <- private$.analyzeMissingPatterns(variable)
                if (length(missing_patterns) > 0) {
                    quality_text <- paste0(quality_text, "• Missing Pattern: ", names(missing_patterns)[1], "\n")
                }
            }
            quality_text <- paste0(quality_text, "\n")
            
            # Distribution and accuracy assessment for numeric variables
            if (is_numeric && n_complete >= 2) {
                clean_var <- variable[!is.na(variable)]
                mean_val <- mean(clean_var)
                sd_val <- sd(clean_var)
                cv <- ifelse(mean_val != 0, abs(sd_val / mean_val) * 100, 0)
                skewness <- ifelse(sd_val > 0, mean((clean_var - mean_val)^3) / sd_val^3, 0)
                
                quality_text <- paste0(quality_text, "📈 DISTRIBUTION ASSESSMENT:\n")
                quality_text <- paste0(quality_text, sprintf("• Central Tendency: Mean = %.3f, Median = %.3f\n", 
                                                            mean_val, median(clean_var)))
                quality_text <- paste0(quality_text, sprintf("• Variability: SD = %.3f, CV = %.1f%%\n", sd_val, cv))
                quality_text <- paste0(quality_text, sprintf("• Distribution Shape: %s (skewness = %.2f)\n", 
                                                            private$.interpretSkewness(skewness), skewness))
                
                if (outliers_found > 0) {
                    quality_text <- paste0(quality_text, sprintf("• Outliers Detected: %d (%.1f%% of data)\n", 
                                                                outliers_found, 100*outlier_rate))
                } else {
                    quality_text <- paste0(quality_text, "• Outliers: None detected (excellent data quality)\n")
                }
                quality_text <- paste0(quality_text, "\n")
            }
            
            # Categorical data assessment
            if (is_categorical && n_complete > 0) {
                categorical_assessment <- private$.analyzeCategoricalQuality(variable)
                quality_text <- paste0(quality_text, "🏷️  CATEGORICAL DATA ASSESSMENT:\n")
                quality_text <- paste0(quality_text, sprintf("• Number of Categories: %d\n", n_unique))
                
                if (!is.null(categorical_assessment)) {
                    if (!is.null(categorical_assessment$balanced)) {
                        quality_text <- paste0(quality_text, "• Category Balance: Good\n")
                    } else if (!is.null(categorical_assessment$moderate_imbalance)) {
                        quality_text <- paste0(quality_text, "• Category Balance: Moderate imbalance detected\n")
                    } else if (!is.null(categorical_assessment$imbalance)) {
                        quality_text <- paste0(quality_text, "• Category Balance: Severe imbalance detected\n")
                    }
                }
                quality_text <- paste0(quality_text, "\n")
            }
            
            # Quality issues summary
            if (length(quality_issues) > 0) {
                quality_text <- paste0(quality_text, "⚠️  QUALITY CONCERNS IDENTIFIED:\n")
                for (issue in quality_issues) {
                    quality_text <- paste0(quality_text, sprintf("• %s\n", stringr::str_to_sentence(issue)))
                }
                quality_text <- paste0(quality_text, "\n")
            }
            
            # Enhanced recommendations based on grade and context
            quality_text <- paste0(quality_text, "💡 RECOMMENDATIONS:\n")
            
            if (quality_grade == "A") {
                quality_text <- paste0(quality_text, "✅ EXCELLENT QUALITY - Ready for Analysis\n")
                quality_text <- paste0(quality_text, "• Data meets high-quality standards for clinical research\n")
                quality_text <- paste0(quality_text, "• Proceed with planned statistical analyses\n")
                quality_text <- paste0(quality_text, "• Consider as reference standard for quality benchmarking\n")
                quality_text <- paste0(quality_text, "• Document quality assessment for regulatory submissions\n")
                
            } else if (quality_grade == "B") {
                quality_text <- paste0(quality_text, "✅ GOOD QUALITY - Minor Issues Noted\n")
                quality_text <- paste0(quality_text, "• Data suitable for analysis with minor limitations\n")
                quality_text <- paste0(quality_text, "• Document identified limitations in study methods\n")
                quality_text <- paste0(quality_text, "• Consider sensitivity analyses for robust conclusions\n")
                quality_text <- paste0(quality_text, "• Monitor quality trends in ongoing data collection\n")
                
            } else if (quality_grade == "C") {
                quality_text <- paste0(quality_text, "⚠️  CONCERNING QUALITY - Intervention Required\n")
                quality_text <- paste0(quality_text, "• Data quality issues require attention before analysis\n")
                quality_text <- paste0(quality_text, "• Implement data cleaning procedures (see specific recommendations below)\n")
                quality_text <- paste0(quality_text, "• Perform sensitivity analyses to assess impact of quality issues\n")
                quality_text <- paste0(quality_text, "• Consider consulting with data management team\n")
                quality_text <- paste0(quality_text, "• Document all data cleaning activities for audit trail\n")
                
            } else {
                quality_text <- paste0(quality_text, "🚨 POOR QUALITY - Major Intervention Required\n")
                quality_text <- paste0(quality_text, "• Significant data quality concerns threaten analysis validity\n")
                quality_text <- paste0(quality_text, "• STOP: Do not proceed with analysis until quality issues resolved\n")
                quality_text <- paste0(quality_text, "• Investigate root causes of data quality problems\n")
                quality_text <- paste0(quality_text, "• Consider additional data collection if feasible\n")
                quality_text <- paste0(quality_text, "• Implement corrective and preventive actions (CAPA)\n")
                quality_text <- paste0(quality_text, "• Obtain quality review approval before proceeding\n")
            }
            
            # Specific actionable recommendations
            quality_text <- paste0(quality_text, "\n🔧 SPECIFIC ACTIONS:\n")
            
            if (missing_pct > 15) {
                quality_text <- paste0(quality_text, "• MISSING DATA: Investigate missing data mechanisms (MCAR/MAR/MNAR)\n")
                quality_text <- paste0(quality_text, "• MISSING DATA: Consider multiple imputation methods for sensitivity analysis\n")
            }
            
            if (outliers_found > 0) {
                quality_text <- paste0(quality_text, "• OUTLIERS: Review each outlier for data entry errors and clinical plausibility\n")
                quality_text <- paste0(quality_text, "• OUTLIERS: Consider robust analysis methods (e.g., rank-based tests)\n")
            }
            
            if (!is.null(clinical_issues) && length(clinical_issues) > 0) {
                quality_text <- paste0(quality_text, "• CLINICAL VALIDATION: Verify measurement units and clinical plausibility\n")
                quality_text <- paste0(quality_text, "• CLINICAL VALIDATION: Review data collection procedures\n")
            }
            
            if (n_total < 30) {
                quality_text <- paste0(quality_text, "• SAMPLE SIZE: Consider collecting additional data for robust analysis\n")
                quality_text <- paste0(quality_text, "• SAMPLE SIZE: Use appropriate methods for small sample sizes\n")
            }
            
            # Add validation warnings if present
            if (length(validation_results$warnings) > 0) {
                quality_text <- paste0(quality_text, "\n⚠️  VALIDATION WARNINGS:\n")
                for (warning in validation_results$warnings) {
                    quality_text <- paste0(quality_text, sprintf("• %s\n", warning))
                }
            }
            
            # Add recommendations if present
            if (length(validation_results$recommendations) > 0) {
                quality_text <- paste0(quality_text, "\n📋 ADDITIONAL RECOMMENDATIONS:\n")
                for (rec in validation_results$recommendations) {
                    quality_text <- paste0(quality_text, sprintf("• %s\n", rec))
                }
            }
            
            quality_text <- paste0(quality_text, "\n═══════════════════════════════════════════════════════\n")
            quality_text <- paste0(quality_text, "Generated by ClinicoPath Data Quality Assessment Module\n")
            quality_text <- paste0(quality_text, sprintf("Assessment Date: %s\n", Sys.Date()))
            quality_text <- paste0(quality_text, "═══════════════════════════════════════════════════════")

            self$results$qualityText$setContent(quality_text)
        }
    )
)