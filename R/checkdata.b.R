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
        
        .run = function() {
            # Input validation
            if (is.null(self$options$var))
                return()
                
            if (nrow(self$data) == 0) 
                stop('Data contains no (complete) rows')

            # Get variable data
            variable <- self$data[[self$options$var]]
            var_name <- self$options$var
            
            # Basic data characteristics
            n_total <- length(variable)
            n_missing <- sum(is.na(variable))
            n_complete <- n_total - n_missing
            n_unique <- length(unique(na.omit(variable)))
            missing_pct <- round(100 * n_missing / n_total, 1)
            unique_pct <- round(100 * n_unique / n_complete, 1)
            
            # Variable type detection
            is_numeric <- is.numeric(variable)
            is_categorical <- is.factor(variable) || is.character(variable)
            
            # Missing value analysis with interpretation
            self$results$missingVals$addRow(rowKey=1, values=list(
                metric="Total Observations",
                value=as.character(n_total),
                interpretation="Sample size for analysis"
            ))
            
            self$results$missingVals$addRow(rowKey=2, values=list(
                metric="Missing Values",
                value=sprintf("%d (%.1f%%)", n_missing, missing_pct),
                interpretation=private$.interpretMissing(missing_pct)
            ))
            
            self$results$missingVals$addRow(rowKey=3, values=list(
                metric="Complete Cases",
                value=sprintf("%d (%.1f%%)", n_complete, 100-missing_pct),
                interpretation=ifelse(n_complete >= 0.8 * n_total, "Adequate for analysis", "May limit analysis power")
            ))
            
            self$results$missingVals$addRow(rowKey=4, values=list(
                metric="Unique Values",
                value=sprintf("%d (%.1f%%)", n_unique, unique_pct),
                interpretation=ifelse(unique_pct > 95, "High variability", ifelse(unique_pct < 10, "Low variability", "Moderate variability"))
            ))

            # Outlier detection for numeric variables
            outliers_found <- 0
            if (self$options$showOutliers && is_numeric && n_complete >= 3) {
                clean_var <- variable[!is.na(variable)]
                z_scores <- scale(clean_var)[,1]
                outlier_indices <- which(abs(z_scores) > 3)
                outliers_found <- length(outlier_indices)
                
                if (outliers_found > 0) {
                    # Get original row numbers
                    original_indices <- which(!is.na(variable))[outlier_indices]
                    
                    for (i in seq_along(outlier_indices)) {
                        self$results$outliers$addRow(rowKey=i, values=list(
                            rowNumber=original_indices[i],
                            value=clean_var[outlier_indices[i]],
                            zscore=round(z_scores[outlier_indices[i]], 2),
                            severity=private$.outlierSeverity(z_scores[outlier_indices[i]])
                        ))
                    }
                }
            }

            # Distribution analysis for numeric variables
            if (self$options$showDistribution && is_numeric && n_complete >= 2) {
                clean_var <- variable[!is.na(variable)]
                
                mean_val <- mean(clean_var)
                median_val <- median(clean_var)
                sd_val <- sd(clean_var)
                
                # Calculate skewness
                skewness <- ifelse(sd_val > 0, 
                    mean((clean_var - mean_val)^3) / sd_val^3, 
                    0)
                
                # Calculate coefficient of variation
                cv <- ifelse(mean_val != 0, abs(sd_val / mean_val) * 100, 0)
                
                self$results$distribution$addRow(rowKey=1, values=list(
                    metric="Mean",
                    value=round(mean_val, 3),
                    interpretation="Central tendency measure"
                ))
                
                self$results$distribution$addRow(rowKey=2, values=list(
                    metric="Median", 
                    value=round(median_val, 3),
                    interpretation=ifelse(abs(mean_val - median_val) / sd_val < 0.2, "Similar to mean", "Different from mean")
                ))
                
                self$results$distribution$addRow(rowKey=3, values=list(
                    metric="Standard Deviation",
                    value=round(sd_val, 3),
                    interpretation=ifelse(cv < 15, "Low variability", ifelse(cv < 35, "Moderate variability", "High variability"))
                ))
                
                self$results$distribution$addRow(rowKey=4, values=list(
                    metric="Skewness",
                    value=round(skewness, 3),
                    interpretation=private$.interpretSkewness(skewness)
                ))
                
                # Range analysis
                min_val <- min(clean_var)
                max_val <- max(clean_var)
                range_val <- max_val - min_val
                
                self$results$distribution$addRow(rowKey=5, values=list(
                    metric="Range",
                    value=round(range_val, 3),
                    interpretation=sprintf("From %.3f to %.3f", min_val, max_val)
                ))
            }

            # Duplicate analysis
            if (self$options$showDuplicates && n_complete > 0) {
                clean_var <- variable[!is.na(variable)]
                freq_table <- table(clean_var)
                duplicates <- freq_table[freq_table > 1]
                
                if (length(duplicates) > 0) {
                    # Sort by frequency (descending)
                    duplicates <- sort(duplicates, decreasing = TRUE)
                    
                    for (i in seq_along(duplicates)) {
                        dup_pct <- round(100 * duplicates[i] / n_complete, 1)
                        self$results$duplicates$addRow(rowKey=i, values=list(
                            value=names(duplicates)[i],
                            count=as.integer(duplicates[i]),
                            percentage=dup_pct
                        ))
                    }
                }
            }

            # Data patterns analysis
            if (self$options$showPatterns) {
                pattern_count <- 1
                
                # Missing data pattern
                if (n_missing > 0) {
                    missing_pattern <- ifelse(n_missing / n_total > 0.1, "systematic", "random")
                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern="Missing Data Pattern",
                        description=sprintf("%.1f%% missing, appears %s", missing_pct, missing_pattern),
                        recommendation=ifelse(missing_pct > 15, "Consider imputation or investigation", "Monitor but likely acceptable")
                    ))
                    pattern_count <- pattern_count + 1
                }
                
                # Uniqueness pattern
                if (n_unique / n_complete < 0.1) {
                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern="Low Uniqueness",
                        description=sprintf("Only %.1f%% unique values", unique_pct),
                        recommendation="Verify if this reflects true data structure"
                    ))
                    pattern_count <- pattern_count + 1
                }
                
                # Outlier pattern
                if (outliers_found > 0.05 * n_complete) {
                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern="High Outlier Rate",
                        description=sprintf("%d outliers (%.1f%%)", outliers_found, 100*outliers_found/n_complete),
                        recommendation="Investigate data collection or measurement issues"
                    ))
                    pattern_count <- pattern_count + 1
                }
            }

            # Generate comprehensive quality assessment summary
            quality_grade <- "A"
            quality_issues <- c()
            
            # Assess overall quality
            if (missing_pct > 30) {
                quality_grade <- "D"
                quality_issues <- c(quality_issues, "extensive missing data")
            } else if (missing_pct > 15) {
                quality_grade <- ifelse(quality_grade == "A", "C", quality_grade)
                quality_issues <- c(quality_issues, "substantial missing data")
            } else if (missing_pct > 5) {
                quality_grade <- ifelse(quality_grade == "A", "B", quality_grade)
            }
            
            if (outliers_found > 0.05 * n_complete) {
                quality_grade <- ifelse(quality_grade %in% c("A", "B"), "C", "D")
                quality_issues <- c(quality_issues, "high outlier rate")
            }
            
            if (n_unique / n_complete < 0.05) {
                quality_grade <- ifelse(quality_grade == "A", "B", quality_grade)
                quality_issues <- c(quality_issues, "low data variability")
            }
            
            # Create summary text with clinical research context
            quality_text <- sprintf("DATA QUALITY ASSESSMENT FOR '%s'\n", toupper(var_name))
            quality_text <- paste0(quality_text, sprintf("Overall Quality Grade: %s\n\n", quality_grade))
            
            quality_text <- paste0(quality_text, "COMPLETENESS ASSESSMENT:\n")
            quality_text <- paste0(quality_text, sprintf("• Data completeness: %.1f%% (%d/%d observations)\n", 
                                                        100-missing_pct, n_complete, n_total))
            quality_text <- paste0(quality_text, sprintf("• Missing data impact: %s\n", 
                                                        private$.interpretMissing(missing_pct)))
            
            if (is_numeric) {
                quality_text <- paste0(quality_text, "\nDATA DISTRIBUTION:\n")
                if (n_complete >= 2) {
                    clean_var <- variable[!is.na(variable)]
                    skewness <- mean((clean_var - mean(clean_var))^3) / sd(clean_var)^3
                    quality_text <- paste0(quality_text, sprintf("• Distribution shape: %s\n", 
                                                                private$.interpretSkewness(skewness)))
                }
                
                if (outliers_found > 0) {
                    quality_text <- paste0(quality_text, sprintf("• Outliers detected: %d (%.1f%% of data)\n", 
                                                                outliers_found, 100*outliers_found/n_complete))
                } else {
                    quality_text <- paste0(quality_text, "• No extreme outliers detected\n")
                }
            }
            
            quality_text <- paste0(quality_text, "\nDATA VARIABILITY:\n")
            quality_text <- paste0(quality_text, sprintf("• Unique values: %d (%.1f%% of complete cases)\n", 
                                                        n_unique, unique_pct))
            
            if (length(quality_issues) > 0) {
                quality_text <- paste0(quality_text, "\nQUALITY CONCERNS:\n")
                for (issue in quality_issues) {
                    quality_text <- paste0(quality_text, sprintf("• %s\n", stringr::str_to_sentence(issue)))
                }
            }
            
            quality_text <- paste0(quality_text, "\nRECOMMENDATIONS:\n")
            if (quality_grade == "A") {
                quality_text <- paste0(quality_text, "• Data quality is excellent - proceed with analysis\n")
            } else if (quality_grade == "B") {
                quality_text <- paste0(quality_text, "• Data quality is good - minor issues noted\n")
                quality_text <- paste0(quality_text, "• Consider documenting limitations in analysis\n")
            } else if (quality_grade == "C") {
                quality_text <- paste0(quality_text, "• Data quality issues require attention\n")
                quality_text <- paste0(quality_text, "• Consider data cleaning or sensitivity analyses\n")
            } else {
                quality_text <- paste0(quality_text, "• Significant data quality concerns identified\n")
                quality_text <- paste0(quality_text, "• Data cleaning strongly recommended before analysis\n")
                quality_text <- paste0(quality_text, "• Consider additional data collection if possible\n")
            }
            
            if (missing_pct > 15) {
                quality_text <- paste0(quality_text, "• Investigate missing data mechanisms\n")
                quality_text <- paste0(quality_text, "• Consider multiple imputation methods\n")
            }
            
            if (outliers_found > 0) {
                quality_text <- paste0(quality_text, "• Review outliers for data entry errors\n")
                quality_text <- paste0(quality_text, "• Consider robust analysis methods\n")
            }

            self$results$qualityText$setContent(quality_text)
        }
    )
)