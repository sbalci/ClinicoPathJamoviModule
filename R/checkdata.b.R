# Enhanced data quality assessment for clinical research
# Provides comprehensive evaluation of data completeness, accuracy, and patterns

checkdataClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "checkdataClass",
    inherit = checkdataBase,
    private = list(

        # Cache for performance optimization
        .data_cache = NULL,

        # Initialize and manage data cache
        .initializeCache = function(variable, var_name) {
            cache_key <- paste0(var_name, "_", length(variable))

            if (is.null(private$.data_cache) || private$.data_cache$key != cache_key) {
                private$.data_cache <- list(
                    key = cache_key,
                    clean_var = variable[!is.na(variable)],
                    freq_table = table(variable, useNA = "no"),
                    n_total = length(variable),
                    n_complete = sum(!is.na(variable)),
                    n_missing = sum(is.na(variable)),
                    n_unique = length(unique(variable[!is.na(variable)])),
                    is_numeric = is.numeric(variable),
                    is_categorical = is.factor(variable) || is.character(variable),
                    variable_name = var_name
                )
                # Calculate derived metrics
                private$.data_cache$missing_pct <- (private$.data_cache$n_missing / private$.data_cache$n_total) * 100
                private$.data_cache$unique_pct <- ifelse(private$.data_cache$n_complete > 0,
                                                        (private$.data_cache$n_unique / private$.data_cache$n_complete) * 100, 0)
            }
            return(private$.data_cache)
        },

        .clearCache = function() {
            private$.data_cache <- NULL
        },

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
        # FIXED: Previous version returned "High" for all values including z~0
        .outlierSeverity = function(zscore) {
            abs_z <- abs(zscore)
            if (abs_z > 4) {
                return("Extreme")
            } else if (abs_z > 3.5) {
                return("Very High")
            } else if (abs_z > 3) {
                return("High")
            } else if (abs_z > 2.5) {
                return("Moderate")
            } else if (abs_z > 2) {
                return("Mild")
            } else {
                return("Not an outlier")  # Should not happen if only called on detected outliers
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
        # IMPROVED: Now supports transformation and shows per-method flags
        .advancedOutlierDetection = function(variable) {
            clean_var <- variable[!is.na(variable)]
            original_var <- clean_var  # Keep original for reporting

            # Small sample handling
            is_small_sample <- length(clean_var) < 10
            if (is_small_sample && length(clean_var) < 3) {
                # Too small for any outlier detection
                return(list(
                    outlier_indices = integer(),
                    methods_used = character(),
                    warning = "Insufficient data (n < 3) for any outlier detection",
                    all_methods = list(),
                    is_small_sample = TRUE,
                    is_informative_only = FALSE
                ))
            }

            # Apply transformation if requested (for right-skewed distributions)
            transform_type <- self$options$outlierTransform
            if (transform_type == "log") {
                if (all(clean_var > 0)) {
                    clean_var <- log(clean_var)
                    transform_applied <- "log"
                } else {
                    transform_applied <- "none (negative values present)"
                }
            } else if (transform_type == "sqrt") {
                if (all(clean_var >= 0)) {
                    clean_var <- sqrt(clean_var)
                    transform_applied <- "sqrt"
                } else {
                    transform_applied <- "none (negative values present)"
                }
            } else {
                transform_applied <- "none"
            }

            outlier_results <- list()

            # Method 1: Z-score (standard approach, sensitive to outliers themselves)
            z_scores <- scale(clean_var)[,1]
            z_outliers <- which(abs(z_scores) > 3)
            outlier_results$zscore <- list(
                indices = z_outliers,
                values = original_var[z_outliers],
                scores = z_scores[z_outliers],
                transformed_scores = z_scores[z_outliers],  # Same as scores for reporting
                method_note = "Assumes normal distribution; inflated by outliers themselves"
            )

            # Method 2: IQR method (robust to skewness and outliers)
            Q1 <- quantile(clean_var, 0.25)
            Q3 <- quantile(clean_var, 0.75)
            IQR_val <- Q3 - Q1
            iqr_outliers <- which(clean_var < (Q1 - 1.5 * IQR_val) | clean_var > (Q3 + 1.5 * IQR_val))
            outlier_results$iqr <- list(
                indices = iqr_outliers,
                values = original_var[iqr_outliers],
                bounds = c(Q1 - 1.5 * IQR_val, Q3 + 1.5 * IQR_val),
                method_note = "Robust to non-normality"
            )

            # Method 3: Modified Z-score (MAD-based, most robust)
            if (length(clean_var) > 3) {
                mad_val <- mad(clean_var, constant = 1.4826)  # Consistency factor for normal distribution
                if (mad_val > 0) {
                    modified_z <- 0.6745 * (clean_var - median(clean_var)) / mad_val
                    mad_outliers <- which(abs(modified_z) > 3.5)
                    outlier_results$mad <- list(
                        indices = mad_outliers,
                        values = original_var[mad_outliers],
                        scores = modified_z[mad_outliers],
                        transformed_scores = modified_z[mad_outliers],  # Same for reporting
                        method_note = "Most robust to outliers and skewness"
                    )
                } else {
                    outlier_results$mad <- NULL
                }
            }

            # Create detection matrix for each data point
            n_methods <- if (is.null(outlier_results$mad)) 2 else 3
            detection_matrix <- matrix(FALSE, nrow = length(clean_var), ncol = 3,
                                      dimnames = list(NULL, c("zscore", "iqr", "mad")))
            detection_matrix[outlier_results$zscore$indices, "zscore"] <- TRUE
            detection_matrix[outlier_results$iqr$indices, "iqr"] <- TRUE
            if (!is.null(outlier_results$mad)) {
                detection_matrix[outlier_results$mad$indices, "mad"] <- TRUE
            }

            # Count detections per point
            detection_count <- rowSums(detection_matrix)

            # IMPROVED: For small samples (3-9), show informative-only results (single-method OK)
            # For larger samples, require consensus (≥2 methods)
            if (is_small_sample) {
                # Informative-only: show any point flagged by at least 1 method
                consensus_outliers <- which(detection_count >= 1)
                is_informative_only <- TRUE
                consensus_note <- "INFORMATIVE ONLY (n<10): Single-method flags shown, not statistically robust"
            } else {
                # Standard consensus: require ≥2 methods
                consensus_outliers <- which(detection_count >= 2)
                is_informative_only <- FALSE
                consensus_note <- "Consensus outliers (≥2 methods)"
            }

            # Store transformed z-scores for severity assessment
            transformed_z_scores <- z_scores  # On transformed scale

            return(list(
                outlier_indices = consensus_outliers,
                detection_count = detection_count[consensus_outliers],
                detection_matrix = detection_matrix[consensus_outliers, , drop = FALSE],
                all_methods = outlier_results,
                methods_used = c("Z-score", "IQR", if(!is.null(outlier_results$mad)) "Modified Z-score (MAD)"),
                transform_applied = transform_applied,
                original_n = length(original_var),
                n_methods = n_methods,
                transformed_z_scores = transformed_z_scores,  # For severity on correct scale
                original_values = original_var,  # For display
                is_small_sample = is_small_sample,
                is_informative_only = is_informative_only,
                consensus_note = consensus_note
            ))
        },
        
        # Enhanced missing data pattern analysis
        # IMPROVED: Now labels heuristics and optionally performs MCAR test
        .analyzeMissingPatterns = function(variable, data_context = NULL) {
            missing_indices <- which(is.na(variable))
            complete_indices <- which(!is.na(variable))
            n_total <- length(variable)
            n_missing <- length(missing_indices)
            n_complete <- length(complete_indices)
            missing_pct <- 100 * n_missing / n_total

            patterns <- list()

            # Optional: Little's MCAR test (requires naniar package and full dataset context)
            if (self$options$mcarTest && !is.null(data_context)) {
                if (requireNamespace("naniar", quietly = TRUE)) {
                    tryCatch({
                        # Note: Little's test requires multivariate data
                        # For single variable, we can't perform it; note this limitation
                        patterns$mcar_note <- "MCAR test requires multivariate context (not available for single variable)"
                    }, error = function(e) {
                        patterns$mcar_error <- paste("MCAR test failed:", e$message)
                    })
                } else {
                    patterns$mcar_unavailable <- "MCAR test unavailable (naniar package not installed)"
                }
            }

            # Pattern 1: HEURISTIC runs test for randomness
            if (n_missing > 0 && n_complete > 0) {
                if (n_missing >= 5 && n_complete >= 5) {
                    missing_binary <- is.na(variable)
                    runs <- rle(missing_binary)
                    n_runs <- length(runs$lengths)

                    # Expected runs under randomness
                    expected_runs <- 2 * n_missing * n_complete / n_total + 1

                    # Approximate variance of runs (Wald-Wolfowitz)
                    runs_var <- (2 * n_missing * n_complete * (2 * n_missing * n_complete - n_total)) /
                                 (n_total^2 * (n_total - 1))
                    runs_se <- sqrt(max(runs_var, 0))

                    # Approximate z-score for runs test
                    if (runs_se > 0) {
                        z_runs <- (n_runs - expected_runs) / runs_se
                        # Two-tailed approximate p-value
                        p_runs <- 2 * pnorm(-abs(z_runs))

                        if (p_runs < 0.05) {
                            if (n_runs < expected_runs) {
                                patterns$clustering <- sprintf(
                                    "HEURISTIC: Missing data appears clustered (runs test p=%.3f, %d vs %.1f expected) — possible systematic cause",
                                    p_runs, n_runs, expected_runs
                                )
                            } else {
                                patterns$alternating <- sprintf(
                                    "HEURISTIC: Missing data alternates (runs test p=%.3f, %d vs %.1f expected) — check data collection pattern",
                                    p_runs, n_runs, expected_runs
                                )
                            }
                        } else {
                            patterns$random <- sprintf(
                                "HEURISTIC: Missing pattern consistent with randomness (runs test p=%.3f)",
                                p_runs
                            )
                        }
                    } else {
                        patterns$random_note <- "HEURISTIC: Runs test variance too small for reliable inference"
                    }
                } else {
                    patterns$insufficient <- sprintf(
                        "HEURISTIC: Insufficient data (n_miss=%d, n_complete=%d) for runs test (need ≥5 each)",
                        n_missing, n_complete
                    )
                }
            }

            # Pattern 2: HEURISTIC monotone missing (dropout pattern)
            if (n_missing > n_total * 0.1) {
                # Check if missing data concentrates at end (dropout)
                last_quarter_start <- round(n_total * 0.75)
                missing_in_last_quarter <- sum(missing_indices > last_quarter_start)
                dropout_prop <- missing_in_last_quarter / n_missing

                # 95% CI for dropout proportion (Wilson score interval)
                if (n_missing > 0) {
                    p_hat <- dropout_prop
                    z <- 1.96
                    denom <- 1 + z^2 / n_missing
                    center <- (p_hat + z^2 / (2 * n_missing)) / denom
                    margin <- z * sqrt(p_hat * (1 - p_hat) / n_missing + z^2 / (4 * n_missing^2)) / denom
                    ci_low <- max(0, center - margin)
                    ci_high <- min(1, center + margin)

                    if (ci_low > 0.5) {
                        patterns$dropout <- sprintf(
                            "HEURISTIC: Likely dropout pattern (%.1f%% of missing in last quarter, 95%% CI: %.1f%%-%.1f%%)",
                            dropout_prop * 100, ci_low * 100, ci_high * 100
                        )
                    } else if (dropout_prop > 0.6) {
                        patterns$possible_dropout <- sprintf(
                            "HEURISTIC: Possible dropout pattern (%.1f%% of missing in last quarter, 95%% CI: %.1f%%-%.1f%%) — CI overlaps 50%%",
                            dropout_prop * 100, ci_low * 100, ci_high * 100
                        )
                    }
                }
            }

            # Pattern 3: Missing data percentage thresholds with context
            if (missing_pct > 50) {
                patterns$severe <- sprintf(
                    "Severe missing data (%.1f%%) — major quality concern; analysis may be biased",
                    missing_pct
                )
            } else if (missing_pct > 20) {
                patterns$substantial <- sprintf(
                    "Substantial missing data (%.1f%%) — investigate MCAR/MAR/MNAR mechanisms",
                    missing_pct
                )
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
        # IMPROVED: Now detects units and uses configurable unit system
        .clinicalContextValidation = function(variable, var_name) {
            if (!is.numeric(variable)) {
                return(NULL)
            }

            # Check if clinical validation is enabled
            if (!self$options$clinicalValidation) {
                return(NULL)
            }

            clean_var <- variable[!is.na(variable)]
            if (length(clean_var) == 0) {
                return(NULL)
            }

            clinical_issues <- list()
            unit_system <- self$options$unitSystem

            # Helper: detect units from data range
            detect_units <- function(var, possible_units) {
                range_val <- range(var, na.rm = TRUE)
                # Returns likely unit based on data range
                # This is heuristic and may need adjustment
                for (unit_info in possible_units) {
                    if (range_val[1] >= unit_info$min && range_val[2] <= unit_info$max) {
                        return(unit_info$name)
                    }
                }
                return("unknown")
            }

            # Age-specific validations (unit-agnostic)
            if (grepl("age", var_name, ignore.case = TRUE)) {
                if (any(clean_var < 0)) {
                    clinical_issues$negative_age <- "PLAUSIBILITY CHECK: Negative age values detected (biologically impossible)"
                }
                if (any(clean_var > 120)) {
                    clinical_issues$extreme_age <- "PLAUSIBILITY CHECK: Age >120 years detected (threshold: 120) — verify data accuracy"
                }
                if (any(clean_var < 1 & clean_var > 0)) {
                    clinical_issues$fractional_age <- "PLAUSIBILITY CHECK: Fractional age <1 detected — verify units (years vs months)"
                }
            }

            # Weight-specific validations with unit detection
            if (grepl("weight", var_name, ignore.case = TRUE)) {
                # Auto-detect or use specified
                if (unit_system == "auto") {
                    weight_units <- detect_units(clean_var, list(
                        list(name = "kg", min = 2, max = 200),
                        list(name = "lbs", min = 5, max = 450)
                    ))
                } else if (unit_system == "metric") {
                    weight_units <- "kg"
                } else {
                    weight_units <- "lbs"
                }

                # Apply appropriate thresholds
                if (weight_units == "kg") {
                    if (any(clean_var < 2)) {
                        clinical_issues$low_weight <- sprintf(
                            "PLAUSIBILITY CHECK: Weight <2 kg detected (assumed %s) — verify units or data entry",
                            weight_units
                        )
                    }
                    if (any(clean_var > 200)) {
                        clinical_issues$high_weight <- sprintf(
                            "PLAUSIBILITY CHECK: Weight >200 kg detected (assumed %s, threshold: 200) — verify accuracy",
                            weight_units
                        )
                    }
                } else if (weight_units == "lbs") {
                    if (any(clean_var < 5)) {
                        clinical_issues$low_weight <- sprintf(
                            "PLAUSIBILITY CHECK: Weight <5 lbs detected (assumed %s) — verify units or data entry",
                            weight_units
                        )
                    }
                    if (any(clean_var > 450)) {
                        clinical_issues$high_weight <- sprintf(
                            "PLAUSIBILITY CHECK: Weight >450 lbs detected (assumed %s, threshold: 450) — verify accuracy",
                            weight_units
                        )
                    }
                } else {
                    clinical_issues$weight_units <- "PLAUSIBILITY CHECK: Could not auto-detect weight units — specify manually or verify range"
                }
            }

            # Height-specific validations with unit detection
            if (grepl("height", var_name, ignore.case = TRUE)) {
                # Auto-detect or use specified
                if (unit_system == "auto") {
                    height_units <- detect_units(clean_var, list(
                        list(name = "cm", min = 50, max = 250),
                        list(name = "meters", min = 0.5, max = 2.5),
                        list(name = "feet", min = 1.5, max = 8)
                    ))
                } else if (unit_system == "metric") {
                    # Could be cm or meters - use range to decide
                    height_units <- ifelse(max(clean_var, na.rm = TRUE) > 10, "cm", "meters")
                } else {
                    height_units <- "feet/inches"
                }

                # Apply appropriate plausibility checks
                if (height_units == "cm") {
                    if (any(clean_var < 50) || any(clean_var > 250)) {
                        clinical_issues$implausible_height <- sprintf(
                            "PLAUSIBILITY CHECK: Height outside 50-250 cm range (assumed %s) — verify units",
                            height_units
                        )
                    }
                } else if (height_units == "meters") {
                    if (any(clean_var < 0.5) || any(clean_var > 2.5)) {
                        clinical_issues$implausible_height <- sprintf(
                            "PLAUSIBILITY CHECK: Height outside 0.5-2.5 m range (assumed %s) — verify units",
                            height_units
                        )
                    }
                } else if (height_units == "feet") {
                    if (any(clean_var < 1.5) || any(clean_var > 8)) {
                        clinical_issues$implausible_height <- sprintf(
                            "PLAUSIBILITY CHECK: Height outside 1.5-8 ft range (assumed %s) — verify units",
                            height_units
                        )
                    }
                } else {
                    clinical_issues$height_units <- "PLAUSIBILITY CHECK: Could not auto-detect height units — specify manually"
                }
            }

            # Laboratory value ranges (assume SI units for metric, traditional for imperial)
            if (grepl("hemoglobin|hgb|hb", var_name, ignore.case = TRUE)) {
                # g/dL is common in US, g/L in SI (multiply by 10)
                # Most data will be in g/dL range (3-20), g/L would be 30-200
                if (any(clean_var > 25)) {
                    # Likely g/L
                    if (any(clean_var < 30) || any(clean_var > 200)) {
                        clinical_issues$hemoglobin_range <- "PLAUSIBILITY CHECK: Hemoglobin outside 30-200 g/L range (assumed SI units, threshold: 30-200) — verify units"
                    }
                } else {
                    # Likely g/dL
                    if (any(clean_var < 3) || any(clean_var > 20)) {
                        clinical_issues$hemoglobin_range <- "PLAUSIBILITY CHECK: Hemoglobin outside 3-20 g/dL range (assumed traditional units, threshold: 3-20) — verify accuracy"
                    }
                }
            }

            if (grepl("creatinine|cr", var_name, ignore.case = TRUE)) {
                # mg/dL (US) typically 0.3-10, µmol/L (SI) typically 30-1000
                if (any(clean_var > 20)) {
                    # Likely µmol/L
                    if (any(clean_var < 30) || any(clean_var > 1000)) {
                        clinical_issues$creatinine_range <- "PLAUSIBILITY CHECK: Creatinine outside 30-1000 µmol/L range (assumed SI units, threshold: 30-1000) — verify units"
                    }
                } else {
                    # Likely mg/dL
                    if (any(clean_var < 0.3) || any(clean_var > 10)) {
                        clinical_issues$creatinine_range <- "PLAUSIBILITY CHECK: Creatinine outside 0.3-10 mg/dL range (assumed traditional units, threshold: 0.3-10) — verify accuracy"
                    }
                }
            }

            return(clinical_issues)
        },

        # Initialize function for setup tasks
        .init = function() {
            # Set initial visibility states
            # This runs once when the analysis is created
        },

        # Populate distribution analysis table
        .populateDistributionAnalysis = function(variable, is_numeric, is_categorical, n_complete) {
            if (!self$options$showDistribution) {
                return()
            }

            if (is_numeric && n_complete >= 2) {
                clean_var <- variable[!is.na(variable)]

                mean_val <- mean(clean_var)
                median_val <- median(clean_var)
                sd_val <- sd(clean_var)
                mad_val <- mad(clean_var, constant = 1.4826)  # Robust spread

                # Enhanced skewness calculation with interpretation
                skewness <- ifelse(sd_val > 0 && length(clean_var) > 2,
                    mean((clean_var - mean_val)^3) / sd_val^3, 0)

                # FIXED: CV calculation with stability check
                cv_min_mean <- self$options$cvMinMean
                cv_valid <- abs(mean_val) >= cv_min_mean
                cv <- ifelse(cv_valid && mean_val != 0, abs(sd_val / mean_val) * 100, NA)

                # Kurtosis calculation (measure of tail heaviness)
                kurtosis <- ifelse(sd_val > 0 && length(clean_var) > 3,
                    mean((clean_var - mean_val)^4) / sd_val^4 - 3, 0)

                self$results$distribution$addRow(rowKey="mean", values=list(
                    metric="Mean",
                    value=round(mean_val, 4),
                    interpretation=ifelse(!is.na(cv) && cv < 10, "Stable central value",
                                        ifelse(!is.na(cv), "Variable central tendency", "Central tendency"))
                ))

                self$results$distribution$addRow(rowKey="median", values=list(
                    metric="Median",
                    value=round(median_val, 4),
                    interpretation=ifelse(abs(mean_val - median_val) / sd_val < 0.2,
                                        "Close to mean (symmetric)", "Different from mean (skewed)")
                ))

                self$results$distribution$addRow(rowKey="std_dev", values=list(
                    metric="Standard Deviation",
                    value=round(sd_val, 4),
                    interpretation=sprintf("Absolute variability (see also MAD: %.3f)", mad_val)
                ))

                # Add MAD as a robust alternative to SD
                self$results$distribution$addRow(rowKey="mad", values=list(
                    metric="MAD (Median Abs. Deviation)",
                    value=round(mad_val, 4),
                    interpretation="Robust spread measure (resistant to outliers)"
                ))

                # CV with improved context
                if (!is.na(cv)) {
                    cv_interpretation <- ifelse(cv < 10, "Low relative variability",
                                        ifelse(cv < 20, "Moderate relative variability",
                                              ifelse(cv < 50, "High relative variability",
                                                    "Very high relative variability")))
                    self$results$distribution$addRow(rowKey="coeff_var", values=list(
                        metric="Coefficient of Variation (%)",
                        value=round(cv, 2),
                        interpretation=paste(cv_interpretation, "— appropriate for ratio scale data")
                    ))
                } else {
                    self$results$distribution$addRow(rowKey="coeff_var", values=list(
                        metric="Coefficient of Variation (%)",
                        value=NA,
                        interpretation=sprintf("Suppressed (|mean| < %.3f); use MAD or IQR for spread", cv_min_mean)
                    ))
                }

                self$results$distribution$addRow(rowKey="skewness", values=list(
                    metric="Skewness",
                    value=round(skewness, 3),
                    interpretation=private$.interpretSkewness(skewness)
                ))

                # Enhanced range analysis with outlier context
                min_val <- min(clean_var)
                max_val <- max(clean_var)
                range_val <- max_val - min_val

                self$results$distribution$addRow(rowKey="range", values=list(
                    metric="Range",
                    value=round(range_val, 4),
                    interpretation=sprintf("From %.3f to %.3f", min_val, max_val)
                ))

                # Add quartile information (robust percentiles)
                q1 <- quantile(clean_var, 0.25)
                q3 <- quantile(clean_var, 0.75)
                iqr <- q3 - q1

                self$results$distribution$addRow(rowKey="iqr", values=list(
                    metric="Interquartile Range (IQR)",
                    value=round(iqr, 4),
                    interpretation=sprintf("Q1: %.3f, Q3: %.3f — robust spread metric", q1, q3)
                ))

            } else if (is_categorical && n_complete >= 1) {
                # Distribution analysis for categorical variables
                clean_var <- variable[!is.na(variable)]
                freq_table <- table(clean_var)
                n_categories <- length(freq_table)

                # Modal category and frequency
                modal_category <- names(which.max(freq_table))
                modal_freq <- max(freq_table)
                modal_pct <- round(100 * modal_freq / n_complete, 1)

                # IMPROVED: Category balance (entropy-based) with scale context
                props <- as.numeric(freq_table) / n_complete
                entropy <- -sum(props * log(props, base = 2))
                max_entropy <- log(n_categories, base = 2)
                balance_index <- ifelse(max_entropy > 0, entropy / max_entropy, 1)

                # IMPROVED: Rare categories using configurable threshold
                rare_threshold_pct <- self$options$rareCategoryThreshold
                rare_threshold_n <- (rare_threshold_pct / 100) * n_complete
                rare_categories <- sum(freq_table < rare_threshold_n)

                self$results$distribution$addRow(rowKey="num_categories", values=list(
                    metric="Number of Categories",
                    value=n_categories,
                    interpretation=ifelse(n_categories <= 5, "Manageable number of categories",
                                        ifelse(n_categories <= 10, "Moderate number of categories",
                                              "Many categories - consider grouping"))
                ))

                self$results$distribution$addRow(rowKey="modal_category", values=list(
                    metric="Modal Category",
                    value=paste0(modal_category, " (", modal_freq, ")"),
                    interpretation=sprintf("Most frequent: %s (%.1f%%)", modal_category, modal_pct)
                ))

                self$results$distribution$addRow(rowKey="balance_index", values=list(
                    metric="Category Balance Index (Entropy)",
                    value=round(balance_index, 3),
                    interpretation=sprintf("%.2f of %.2f max entropy; %s",
                                          entropy, max_entropy,
                                          ifelse(balance_index > 0.8, "well balanced",
                                                ifelse(balance_index > 0.6, "moderately balanced", "imbalanced")))
                ))

                if (rare_categories > 0) {
                    self$results$distribution$addRow(rowKey="rare_categories", values=list(
                        metric="Rare Categories",
                        value=rare_categories,
                        interpretation=sprintf("%d categories with <%.1f%% frequency — may violate chi-squared assumptions (expected cell count ≥5)",
                                              rare_categories, rare_threshold_pct)
                    ))
                }

                # Dominant category concern
                if (modal_pct > 80) {
                    self$results$distribution$addRow(rowKey="dominance_warning", values=list(
                        metric="Dominance Warning",
                        value=paste0(modal_pct, "%"),
                        interpretation="One category dominates - consider data quality"
                    ))
                }
            }
        },

        # Populate outlier analysis
        # IMPROVED: Now shows per-method flags and method summary
        .populateOutlierAnalysis = function(variable, is_numeric, n_complete) {
            if (!self$options$showOutliers) {
                return(0)
            }

            outliers_found <- 0

            if (is_numeric && n_complete >= 3) {
                outlier_analysis <- private$.advancedOutlierDetection(variable)
                outliers_found <- length(outlier_analysis$outlier_indices)

                # Show informative-only warning if small sample
                if (!is.null(outlier_analysis$is_informative_only) && outlier_analysis$is_informative_only) {
                    # Update table title to show informative-only status
                    self$results$outliers$setTitle(paste0("Outlier Detection - ", outlier_analysis$consensus_note))
                    self$results$outlierMethodSummary$setTitle("Method Summary (INFORMATIVE ONLY - n<10)")
                }

                # Populate method summary table (always shown when outlier analysis runs)
                self$results$outlierMethodSummary$setVisible(TRUE)

                # Add Z-score method
                self$results$outlierMethodSummary$addRow(rowKey="zscore", values=list(
                    method="Z-score",
                    threshold="|z| > 3",
                    outliers_detected=length(outlier_analysis$all_methods$zscore$indices),
                    note=outlier_analysis$all_methods$zscore$method_note
                ))

                # Add IQR method
                self$results$outlierMethodSummary$addRow(rowKey="iqr", values=list(
                    method="IQR (1.5×IQR)",
                    threshold="< Q1-1.5×IQR or > Q3+1.5×IQR",
                    outliers_detected=length(outlier_analysis$all_methods$iqr$indices),
                    note=outlier_analysis$all_methods$iqr$method_note
                ))

                # Add MAD method if available
                if (!is.null(outlier_analysis$all_methods$mad)) {
                    self$results$outlierMethodSummary$addRow(rowKey="mad", values=list(
                        method="Modified Z-score (MAD)",
                        threshold="|modified-z| > 3.5",
                        outliers_detected=length(outlier_analysis$all_methods$mad$indices),
                        note=outlier_analysis$all_methods$mad$method_note
                    ))
                }

                # Add transformation info if applied
                if (outlier_analysis$transform_applied != "none") {
                    self$results$outlierMethodSummary$addRow(rowKey="transform", values=list(
                        method="Transformation",
                        threshold=outlier_analysis$transform_applied,
                        outliers_detected=NA,
                        note="Applied before outlier detection to handle skewness"
                    ))
                }

                if (outliers_found > 0) {
                    # Show outliers table, hide no outliers message
                    self$results$outliers$setVisible(TRUE)
                    self$results$noOutliers$setVisible(FALSE)

                    # Get original row numbers
                    complete_indices <- which(!is.na(variable))

                    for (i in seq_along(outlier_analysis$outlier_indices)) {
                        outlier_idx <- outlier_analysis$outlier_indices[i]
                        original_row <- complete_indices[outlier_idx]

                        # FIXED: Use transformed z-score for severity
                        z_score_transformed <- outlier_analysis$transformed_z_scores[outlier_idx]
                        confidence_level <- outlier_analysis$detection_count[i]

                        # Build severity text with scale notation
                        scale_note <- if (outlier_analysis$transform_applied != "none") {
                            paste0(" on ", outlier_analysis$transform_applied, " scale")
                        } else {
                            ""
                        }

                        severity_text <- paste0(private$.outlierSeverity(z_score_transformed),
                                              " (", confidence_level, "/", outlier_analysis$n_methods, " methods",
                                              scale_note, ")")

                        # Per-method flags
                        method_flags <- outlier_analysis$detection_matrix[i, ]
                        zscore_flag <- ifelse(method_flags["zscore"], "✓", "—")
                        iqr_flag <- ifelse(method_flags["iqr"], "✓", "—")
                        mad_flag <- if (is.null(outlier_analysis$all_methods$mad)) {
                            "N/A"
                        } else {
                            ifelse(method_flags["mad"], "✓", "—")
                        }

                        self$results$outliers$addRow(rowKey=i, values=list(
                            rowNumber=original_row,
                            value=outlier_analysis$original_values[outlier_idx],
                            zscore=round(z_score_transformed, 3),
                            zscoreFlag=zscore_flag,
                            iqrFlag=iqr_flag,
                            madFlag=mad_flag,
                            severity=severity_text
                        ))
                    }
                } else {
                    # No outliers detected - show confirmation message, hide table
                    self$results$outliers$setVisible(FALSE)
                    self$results$noOutliers$setVisible(TRUE)
                    transform_note <- if (outlier_analysis$transform_applied != "none") {
                        paste0(" (after ", outlier_analysis$transform_applied, " transformation)")
                    } else {
                        ""
                    }
                    no_outliers_message <- paste0(
                        "<p style='color: #28a745; font-weight: bold;'>No consensus outliers detected</p>",
                        "<p>No values were flagged by ≥2 methods", transform_note, ". ",
                        "See Method Summary table for individual method results. ",
                        "This indicates good data quality for this variable.</p>"
                    )
                    self$results$noOutliers$setContent(no_outliers_message)
                }
            } else if (is_numeric && n_complete < 3) {
                # Insufficient data for outlier detection
                self$results$outliers$setVisible(FALSE)
                self$results$outlierMethodSummary$setVisible(FALSE)
                self$results$noOutliers$setVisible(TRUE)
                no_outliers_message <- paste0("<p style='color: #ffc107; font-weight: bold;'>Insufficient data for outlier detection</p><p><strong>HEURISTIC LIMITATION:</strong> At least 3 complete observations are required for outlier detection. Current n=", n_complete, ".</p>")
                self$results$noOutliers$setContent(no_outliers_message)
            } else if (!is_numeric) {
                # Non-numeric variables - explain why outlier detection isn't applicable
                self$results$outliers$setVisible(FALSE)
                self$results$outlierMethodSummary$setVisible(FALSE)
                self$results$noOutliers$setVisible(TRUE)
                no_outliers_message <- "<p style='color: #17a2b8; font-weight: bold;'>Outlier detection not applicable</p><p>Outlier analysis is only available for numeric variables. This variable is categorical.</p>"
                self$results$noOutliers$setContent(no_outliers_message)
            }

            return(outliers_found)
        },

        # Populate missing data analysis
        .populateMissingDataAnalysis = function(variable, n_total) {
            n_missing <- sum(is.na(variable))
            n_complete <- n_total - n_missing
            missing_pct <- (n_missing / n_total) * 100
            n_unique <- length(unique(variable[!is.na(variable)]))
            unique_pct <- ifelse(n_complete > 0, (n_unique / n_complete) * 100, 0)

            self$results$missingVals$addRow(rowKey="total_obs", values=list(
                metric="Total Observations",
                value=as.character(n_total),
                interpretation=ifelse(n_total >= 100, "Adequate sample size",
                                    ifelse(n_total >= 30, "Moderate sample size", "Small sample size"))
            ))

            self$results$missingVals$addRow(rowKey="missing_vals", values=list(
                metric="Missing Values",
                value=sprintf("%d (%.1f%%)", n_missing, missing_pct),
                interpretation=private$.interpretMissing(missing_pct)
            ))

            self$results$missingVals$addRow(rowKey="complete_cases", values=list(
                metric="Complete Cases",
                value=sprintf("%d (%.1f%%)", n_complete, 100-missing_pct),
                interpretation=ifelse(n_complete >= 0.9 * n_total, "Excellent completeness",
                                    ifelse(n_complete >= 0.8 * n_total, "Good completeness",
                                          ifelse(n_complete >= 0.7 * n_total, "Acceptable completeness", "Poor completeness")))
            ))

            self$results$missingVals$addRow(rowKey="unique_vals", values=list(
                metric="Unique Values",
                value=sprintf("%d (%.1f%%)", n_unique, unique_pct),
                interpretation=ifelse(unique_pct > 95, "Very high variability",
                                    ifelse(unique_pct > 50, "High variability",
                                          ifelse(unique_pct > 10, "Moderate variability", "Low variability")))
            ))

            return(list(
                n_missing = n_missing,
                n_complete = n_complete,
                missing_pct = missing_pct,
                n_unique = n_unique,
                unique_pct = unique_pct
            ))
        },

        # Populate duplicate analysis
        .populateDuplicateAnalysis = function(variable, n_complete) {
            if (!self$options$showDuplicates || n_complete == 0) {
                return()
            }

            clean_var <- variable[!is.na(variable)]
            freq_table <- table(clean_var)
            is_categorical <- is.factor(variable) || is.character(variable)

            # Enhanced duplicate detection
            if (is_categorical) {
                # For categorical data, show all frequencies
                freq_table_sorted <- sort(freq_table, decreasing = TRUE)
                max_display <- min(20, length(freq_table_sorted))  # Limit display

                for (i in 1:max_display) {
                    value_name <- names(freq_table_sorted)[i]
                    count <- freq_table_sorted[i]
                    percentage <- round((count / n_complete) * 100, 1)

                    self$results$duplicates$addRow(rowKey=paste0("cat_", i), values=list(
                        value=value_name,
                        count=as.integer(count),
                        percentage=percentage
                    ))
                }
            } else {
                # For numeric data, only show values that appear more than once
                duplicates <- freq_table[freq_table > 1]
                if (length(duplicates) > 0) {
                    duplicates_sorted <- sort(duplicates, decreasing = TRUE)
                    max_display <- min(15, length(duplicates_sorted))

                    for (i in 1:max_display) {
                        value_name <- names(duplicates_sorted)[i]
                        count <- duplicates_sorted[i]
                        percentage <- round((count / n_complete) * 100, 1)

                        self$results$duplicates$addRow(rowKey=paste0("dup_", i), values=list(
                            value=value_name,
                            count=as.integer(count),
                            percentage=percentage
                        ))
                    }
                }
            }
        },

        .run = function() {
            # Control visibility based on variable selection
            variable_selected <- !is.null(self$options$var)

            # Set visibility for all items
            self$results$todo$setVisible(!variable_selected)
            self$results$qualityText$setVisible(variable_selected)
            self$results$missingVals$setVisible(variable_selected)
            # Outlier visibility will be controlled dynamically based on results
            self$results$noOutliers$setVisible(FALSE)  # Initially hidden, shown when no outliers
            self$results$outliers$setVisible(FALSE)    # Initially hidden, shown when outliers found
            self$results$distribution$setVisible(variable_selected && self$options$showDistribution)
            self$results$duplicates$setVisible(variable_selected && self$options$showDuplicates)
            self$results$patterns$setVisible(variable_selected && self$options$showPatterns)

            # Enhanced input validation with user guidance
            if (is.null(self$options$var)) {
                todo_content <- "
                <h3>📊 ClinicoPath Data Quality Assessment</h3>
                <p><strong>Purpose:</strong> Comprehensive evaluation of data completeness, accuracy, and patterns for clinical research.</p>

                <p><strong>⚠️ IMPORTANT:</strong> Outlier detection uses a <strong>consensus approach</strong> -
                points are only flagged if detected by ≥2 of 3 methods (Z-score, IQR, Modified Z-score).
                Points flagged by only 1 method are <strong>not shown</strong>, even if they exceed |z|>3.</p>

                <h4>Required Input:</h4>
                <ul>
                    <li><strong>Variable to Check:</strong> Select any variable for quality assessment</li>
                </ul>

                <h4>⚙️ Analysis Options:</h4>
                <ul>
                    <li><strong>Outlier Analysis:</strong> Consensus-based detection (≥2 of 3 methods: Z-score |z|>3, IQR 1.5×rule, Modified Z-score |z|>3.5)</li>
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
                "
                self$results$todo$setContent(todo_content)
                return()
            }
            
            # Enhanced error checking and validation
            if (nrow(self$data) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'emptyDataset',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Dataset contains no rows. Please provide data for quality assessment.')
                self$results$insert(999, notice)
                return()
            }

            # Get variable data with enhanced validation
            variable <- self$data[[self$options$var]]
            var_name <- self$options$var
            
            # Comprehensive data validation
            validation_results <- private$.validateData(variable, var_name)
            
            # Handle validation errors
            if (!validation_results$is_valid) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'validationError',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(paste("Data Validation Error:", paste(validation_results$error_messages, collapse = "; ")))
                self$results$insert(999, notice)
                return()
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
            self$results$missingVals$addRow(rowKey="total_obs", values=list(
                metric="Total Observations",
                value=as.character(n_total),
                interpretation=ifelse(n_total >= 100, "Adequate sample size",
                                    ifelse(n_total >= 30, "Moderate sample size", "Small sample size"))
            ))

            self$results$missingVals$addRow(rowKey="missing_vals", values=list(
                metric="Missing Values",
                value=sprintf("%d (%.1f%%)", n_missing, missing_pct),
                interpretation=private$.interpretMissing(missing_pct)
            ))

            self$results$missingVals$addRow(rowKey="complete_cases", values=list(
                metric="Complete Cases",
                value=sprintf("%d (%.1f%%)", n_complete, 100-missing_pct),
                interpretation=ifelse(n_complete >= 0.9 * n_total, "Excellent completeness",
                                    ifelse(n_complete >= 0.8 * n_total, "Good completeness",
                                          ifelse(n_complete >= 0.7 * n_total, "Acceptable completeness", "Poor completeness")))
            ))

            self$results$missingVals$addRow(rowKey="unique_vals", values=list(
                metric="Unique Values",
                value=sprintf("%d (%.1f%%)", n_unique, unique_pct),
                interpretation=ifelse(unique_pct > 95, "Very high variability", 
                                    ifelse(unique_pct > 50, "High variability",
                                          ifelse(unique_pct > 10, "Moderate variability", "Low variability")))
            ))

            # Advanced outlier detection for numeric variables
            # Use the refactored .populateOutlierAnalysis method
            outliers_found <- private$.populateOutlierAnalysis(variable, is_numeric, n_complete)

            # Enhanced distribution analysis for numeric and categorical variables
            # Use the refactored .populateDistributionAnalysis method
            if (self$options$showDistribution) {
                private$.populateDistributionAnalysis(variable, is_numeric, is_categorical, n_complete)
            }

            # Keep categorical distribution for .run method (legacy support)
            if (FALSE && self$options$showDistribution && is_categorical && n_complete >= 1) {
                # Distribution analysis for categorical variables
                clean_var <- variable[!is.na(variable)]
                freq_table <- table(clean_var)
                n_categories <- length(freq_table)

                # Modal category and frequency
                modal_category <- names(which.max(freq_table))
                modal_freq <- max(freq_table)
                modal_pct <- round(100 * modal_freq / n_complete, 1)

                # IMPROVED: Category balance (entropy-based) with scale context
                props <- as.numeric(freq_table) / n_complete
                entropy <- -sum(props * log(props, base = 2))
                max_entropy <- log(n_categories, base = 2)
                balance_index <- ifelse(max_entropy > 0, entropy / max_entropy, 1)

                # IMPROVED: Rare categories using configurable threshold
                rare_threshold_pct <- self$options$rareCategoryThreshold
                rare_threshold_n <- (rare_threshold_pct / 100) * n_complete
                rare_categories <- sum(freq_table < rare_threshold_n)

                self$results$distribution$addRow(rowKey="num_categories", values=list(
                    metric="Number of Categories",
                    value=n_categories,
                    interpretation=ifelse(n_categories <= 5, "Manageable number of categories",
                                        ifelse(n_categories <= 10, "Moderate number of categories",
                                              "Many categories - consider grouping"))
                ))

                self$results$distribution$addRow(rowKey="modal_category", values=list(
                    metric="Modal Category",
                    value=paste0(modal_category, " (", modal_freq, ")"),
                    interpretation=sprintf("Most frequent: %s (%.1f%%)", modal_category, modal_pct)
                ))

                self$results$distribution$addRow(rowKey="balance_index", values=list(
                    metric="Category Balance Index (Entropy)",
                    value=round(balance_index, 3),
                    interpretation=sprintf("%.2f of %.2f max entropy; %s",
                                          entropy, max_entropy,
                                          ifelse(balance_index > 0.8, "well balanced",
                                                ifelse(balance_index > 0.6, "moderately balanced", "imbalanced")))
                ))

                if (rare_categories > 0) {
                    self$results$distribution$addRow(rowKey="rare_categories", values=list(
                        metric="Rare Categories",
                        value=rare_categories,
                        interpretation=sprintf("%d categories with <%.1f%% frequency — may violate chi-squared assumptions (expected cell count ≥5)",
                                              rare_categories, rare_threshold_pct)
                    ))
                }

                # Dominant category concern
                if (modal_pct > 80) {
                    self$results$distribution$addRow(rowKey="dominance_warning", values=list(
                        metric="Dominance Warning",
                        value=paste0(modal_pct, "%"),
                        interpretation="One category dominates - consider data quality"
                    ))
                }
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

                # Additional patterns for numeric variables
                if (is_numeric && n_complete >= 2) {
                    clean_var <- variable[!is.na(variable)]

                    # Distribution shape pattern
                    if (length(clean_var) > 3) {
                        skewness <- ifelse(sd(clean_var) > 0,
                            mean((clean_var - mean(clean_var))^3) / sd(clean_var)^3, 0)

                        if (abs(skewness) > 1) {
                            skew_direction <- ifelse(skewness > 0, "right-skewed", "left-skewed")
                            self$results$patterns$addRow(rowKey=pattern_count, values=list(
                                pattern="Distribution Shape",
                                description=sprintf("Highly %s distribution (skewness: %.2f)", skew_direction, skewness),
                                recommendation="Consider data transformation or non-parametric methods"
                            ))
                            pattern_count <- pattern_count + 1
                        }
                    }

                    # Range and precision patterns
                    range_val <- max(clean_var) - min(clean_var)
                    if (all(clean_var == round(clean_var))) {
                        self$results$patterns$addRow(rowKey=pattern_count, values=list(
                            pattern="Data Precision",
                            description="All values are integers (whole numbers)",
                            recommendation="Verify if decimal precision is needed for analysis"
                        ))
                        pattern_count <- pattern_count + 1
                    }

                    # Concentration patterns (clustering)
                    if (length(unique(clean_var)) < n_complete * 0.5 && length(unique(clean_var)) > 2) {
                        concentration_pct <- round(100 * length(unique(clean_var)) / n_complete, 1)
                        self$results$patterns$addRow(rowKey=pattern_count, values=list(
                            pattern="Value Concentration",
                            description=sprintf("Moderate value clustering: %.1f%% unique values", concentration_pct),
                            recommendation="Check for rounding, grouping, or measurement intervals"
                        ))
                        pattern_count <- pattern_count + 1
                    }
                }

                # If no patterns were found, add a general assessment
                if (pattern_count == 1) {
                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern="Overall Assessment",
                        description="No significant data quality issues detected",
                        recommendation="Data appears suitable for standard statistical analysis"
                    ))
                }
            }

            # IMPROVED: Transparent heuristic quality scoring
            quality_grade <- "A"
            quality_issues <- c()
            quality_score <- 100  # Start with perfect score

            # Component scores for transparency
            component_scores <- list()

            # Component 1: Missing data assessment (max penalty: 40 points)
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
            component_scores$missing <- list(penalty = missing_penalty, max_penalty = 40,
                                             description = sprintf("Missing %.1f%%", missing_pct))
            quality_score <- quality_score - missing_penalty

            # Component 2: Outlier assessment (max penalty: 30 points)
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
            component_scores$outliers <- list(penalty = outlier_penalty, max_penalty = 30,
                                              description = sprintf("Outlier rate %.1f%%", outlier_rate * 100))
            quality_score <- quality_score - outlier_penalty

            # Component 3: Variability assessment (max penalty: 25 points)
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
            component_scores$variability <- list(penalty = variability_penalty, max_penalty = 25,
                                                 description = sprintf("Uniqueness %.1f%%", uniqueness_ratio * 100))
            quality_score <- quality_score - variability_penalty

            # Component 4: Clinical validity assessment (max penalty: 20 points, if enabled)
            clinical_penalty <- 0
            clinical_issues_found <- private$.clinicalContextValidation(variable, var_name)
            if (!is.null(clinical_issues_found) && length(clinical_issues_found) > 0) {
                penalizable <- clinical_issues_found[!grepl("auto-detect|could not auto", clinical_issues_found, ignore.case = TRUE)]
                if (length(penalizable) > 0) {
                    clinical_penalty <- min(20, length(penalizable) * 5)
                    quality_issues <- c(quality_issues, sprintf("clinical plausibility issues (%d checks failed)", length(penalizable)))
                } else {
                    quality_issues <- c(quality_issues, "clinical units unclear (not penalized)")
                }
            }
            component_scores$clinical <- list(penalty = clinical_penalty, max_penalty = 20,
                                              description = sprintf("%d plausibility checks failed",
                                                                   ifelse(is.null(clinical_issues_found), 0, length(clinical_issues_found))))
            quality_score <- quality_score - clinical_penalty

            # Component 5: Sample size assessment (max penalty: 30 points)
            sample_penalty <- 0
            if (n_total < 10) {
                sample_penalty <- 30
                quality_issues <- c(quality_issues, "very small sample size (n<10)")
            } else if (n_total < 30) {
                sample_penalty <- 15
                quality_issues <- c(quality_issues, "small sample size (n<30)")
            } else if (n_total < 50) {
                sample_penalty <- 5
            }
            component_scores$sample_size <- list(penalty = sample_penalty, max_penalty = 30,
                                                 description = sprintf("n=%d", n_total))
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

            # Collect quality threshold Notices for user-facing alerts
            quality_notices <- list()

            # STRONG_WARNING: Severe missing data (>30%)
            if (missing_pct > 30) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'severeMissingData',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf("Severe missing data: %.1f%% missing values. Results may be unreliable; investigate missing data mechanisms (MCAR/MAR/MNAR) before analysis.", missing_pct))
                quality_notices$severeMissing <- notice
            } else if (missing_pct > 15) {
                # WARNING: Substantial missing data (15-30%)
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'substantialMissingData',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf("Substantial missing data: %.1f%% missing values. Consider sensitivity analysis with multiple imputation methods.", missing_pct))
                quality_notices$substantialMissing <- notice
            }

            # STRONG_WARNING: Very high outlier rate (>15%)
            outlier_rate <- ifelse(n_complete > 0, outliers_found / n_complete, 0)
            if (outlier_rate > 0.15) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'veryHighOutlierRate',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf("Very high outlier rate: %.1f%% of data flagged as outliers. Verify measurement procedures and consider robust analysis methods.", outlier_rate * 100))
                quality_notices$veryHighOutliers <- notice
            } else if (outlier_rate > 0.10) {
                # WARNING: High outlier rate (10-15%)
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'highOutlierRate',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf("High outlier rate: %.1f%% of data flagged as outliers. Review each outlier for data entry errors and clinical plausibility.", outlier_rate * 100))
                quality_notices$highOutliers <- notice
            }

            # STRONG_WARNING: Very small sample (n<10)
            if (n_total < 10) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'verySmallSample',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf("Very small sample size (n=%d). Statistical analyses unreliable; outlier detection is informative-only. Consider collecting additional data.", n_total))
                quality_notices$verySmallSample <- notice
            } else if (n_total < 30) {
                # WARNING: Small sample (n<30)
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'smallSample',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf("Small sample size (n=%d). Use appropriate methods for small samples and consider collecting additional data for robust analysis.", n_total))
                quality_notices$smallSample <- notice
            }

            # STRONG_WARNING: Extremely low variability (<1% unique)
            uniqueness_ratio <- ifelse(n_complete > 0, n_unique / n_complete, 0)
            if (uniqueness_ratio < 0.01) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'extremelyLowVariability',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf("Extremely low variability: <1%% unique values (%d unique out of %d). Investigate constant value cause or data collection procedures.", n_unique, n_complete))
                quality_notices$extremeLowVar <- notice
            }

            # WARNING: Clinical plausibility issues (if enabled and issues found)
            if (self$options$clinicalValidation && !is.null(clinical_issues_found) && length(clinical_issues_found) > 0) {
                penalizable <- clinical_issues_found[!grepl("auto-detect|could not auto", clinical_issues_found, ignore.case = TRUE)]
                if (length(penalizable) > 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'clinicalPlausibility',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf("Clinical plausibility issues: %d validation checks failed. Verify measurement units and clinical plausibility before analysis.", length(penalizable)))
                    quality_notices$clinicalIssues <- notice
                }
            }

            # INFO: Analysis complete with quality summary
            grade_desc <- ifelse(quality_score >= 90, "Excellent",
                         ifelse(quality_score >= 80, "Good",
                         ifelse(quality_score >= 70, "Fair", "Poor")))
            notice_info <- jmvcore::Notice$new(
                options = self$options,
                name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO
            )
            notice_info$setContent(sprintf("Quality assessment completed: %d observations analyzed. Overall quality: %s (Grade %s). Note: Scoring is heuristic-based; review component breakdown for details.", n_total, grade_desc, quality_grade))
            quality_notices$analysisComplete <- notice_info

            # Insert notices in priority order: STRONG_WARNING → WARNING → INFO
            position <- 1
            priority_order <- c('severeMissing', 'substantialMissing', 'veryHighOutliers', 'highOutliers',
                               'verySmallSample', 'smallSample', 'extremeLowVar', 'clinicalIssues', 'analysisComplete')
            for (name in priority_order) {
                if (!is.null(quality_notices[[name]])) {
                    self$results$insert(position, quality_notices[[name]])
                    position <- position + 1
                }
            }

            # IMPROVED: Transparent heuristic quality summary with softened presentation
            quality_text <- sprintf("═══════════════════════════════════════════════════════\n")
            quality_text <- paste0(quality_text, sprintf("   DATA QUALITY ASSESSMENT FOR '%s'\n", toupper(var_name)))
            quality_text <- paste0(quality_text, sprintf("═══════════════════════════════════════════════════════\n\n"))

            # Soften presentation: show band instead of precise /100 score
            score_band <- if (quality_score >= 90) {
                "Excellent (90-100)"
            } else if (quality_score >= 80) {
                "Good (80-89)"
            } else if (quality_score >= 70) {
                "Fair (70-79)"
            } else {
                "Poor (<70)"
            }

            quality_text <- paste0(quality_text, sprintf("📊 HEURISTIC QUALITY: Grade %s — %s\n", quality_grade, score_band))
            quality_text <- paste0(quality_text, "\n⚠️  IMPORTANT: This is a HEURISTIC (rule-of-thumb) assessment, NOT a validated metric.\n")
            quality_text <- paste0(quality_text, "The score uses arbitrary thresholds. Apply clinical judgment, not automated rules.\n\n")

            # Show scoring breakdown for transparency
            quality_text <- paste0(quality_text, "SCORING BREAKDOWN (shows penalty applied / maximum penalty):\n")
            quality_text <- paste0(quality_text, sprintf("• Missing Data:      -%2d / %2d pts  (%s)\n",
                                                        component_scores$missing$penalty,
                                                        component_scores$missing$max_penalty,
                                                        component_scores$missing$description))
            quality_text <- paste0(quality_text, sprintf("• Outliers:          -%2d / %2d pts  (%s)\n",
                                                        component_scores$outliers$penalty,
                                                        component_scores$outliers$max_penalty,
                                                        component_scores$outliers$description))
            quality_text <- paste0(quality_text, sprintf("• Variability:       -%2d / %2d pts  (%s)\n",
                                                        component_scores$variability$penalty,
                                                        component_scores$variability$max_penalty,
                                                        component_scores$variability$description))
            quality_text <- paste0(quality_text, sprintf("• Clinical Checks:   -%2d / %2d pts  (%s)\n",
                                                        component_scores$clinical$penalty,
                                                        component_scores$clinical$max_penalty,
                                                        component_scores$clinical$description))
            quality_text <- paste0(quality_text, sprintf("• Sample Size:       -%2d / %2d pts  (%s)\n",
                                                        component_scores$sample_size$penalty,
                                                        component_scores$sample_size$max_penalty,
                                                        component_scores$sample_size$description))
            quality_text <- paste0(quality_text, sprintf("                     ────────────────\n"))
            quality_text <- paste0(quality_text, sprintf("  HEURISTIC GRADE:   %s (%s)\n\n",
                                                        quality_grade, score_band))
            
            # Variable type and basic characteristics
            var_type_desc <- ifelse(is_numeric, "Numeric/Continuous", 
                                   ifelse(is_categorical, "Categorical/Factor", "Other"))
            quality_text <- paste0(quality_text, sprintf("VARIABLE CHARACTERISTICS:\n"))
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
                mad_val <- mad(clean_var, constant = 1.4826)

                # FIXED: Apply cvMinMean guard consistently
                cv_min_mean <- self$options$cvMinMean
                cv_valid <- abs(mean_val) >= cv_min_mean
                cv <- if (cv_valid && mean_val != 0) {
                    abs(sd_val / mean_val) * 100
                } else {
                    NA
                }

                skewness <- ifelse(sd_val > 0, mean((clean_var - mean_val)^3) / sd_val^3, 0)

                quality_text <- paste0(quality_text, "📈 DISTRIBUTION ASSESSMENT:\n")
                quality_text <- paste0(quality_text, sprintf("• Central Tendency: Mean = %.3f, Median = %.3f\n",
                                                            mean_val, median(clean_var)))

                # Show CV or explain suppression
                if (!is.na(cv)) {
                    quality_text <- paste0(quality_text, sprintf("• Variability: SD = %.3f, MAD = %.3f, CV = %.1f%%\n",
                                                                sd_val, mad_val, cv))
                } else {
                    quality_text <- paste0(quality_text, sprintf("• Variability: SD = %.3f, MAD = %.3f (CV suppressed: |mean| < %.3f)\n",
                                                                sd_val, mad_val, cv_min_mean))
                }

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
                quality_text <- paste0(quality_text, "QUALITY CONCERNS IDENTIFIED:\n")
                for (issue in quality_issues) {
                    quality_text <- paste0(quality_text, sprintf("• %s\n", stringr::str_to_sentence(issue)))
                }
                quality_text <- paste0(quality_text, "\n")
            }
            
            # Enhanced recommendations based on grade and context
            quality_text <- paste0(quality_text, "💡 RECOMMENDATIONS:\n")
            
            if (quality_grade == "A") {
                quality_text <- paste0(quality_text, "INTERPRETATION: High-Quality Data (by heuristic rules)\n")
                quality_text <- paste0(quality_text, "• Data appears suitable for planned analyses\n")
                quality_text <- paste0(quality_text, "• Few quality concerns based on automated checks\n")
                quality_text <- paste0(quality_text, "• Document this quality assessment in study methods\n")
                quality_text <- paste0(quality_text, "• Consider as baseline for quality monitoring\n")

            } else if (quality_grade == "B") {
                quality_text <- paste0(quality_text, "INTERPRETATION: Good Quality with Minor Issues (by heuristic rules)\n")
                quality_text <- paste0(quality_text, "• Data likely suitable for analysis with documented limitations\n")
                quality_text <- paste0(quality_text, "• Note identified issues in study methods section\n")
                quality_text <- paste0(quality_text, "• Consider sensitivity analyses for robust conclusions\n")
                quality_text <- paste0(quality_text, "• Monitor quality trends in ongoing data collection\n")

            } else if (quality_grade == "C") {
                quality_text <- paste0(quality_text, "INTERPRETATION: Quality Concerns Detected (by heuristic rules)\n")
                quality_text <- paste0(quality_text, "• Data quality issues may affect analysis validity\n")
                quality_text <- paste0(quality_text, "• Review specific issues below and consider cleaning\n")
                quality_text <- paste0(quality_text, "• Perform sensitivity analyses to assess impact\n")
                quality_text <- paste0(quality_text, "• Consult with data management or statistician\n")
                quality_text <- paste0(quality_text, "• Document all cleaning decisions and rationale\n")

            } else {
                quality_text <- paste0(quality_text, "INTERPRETATION: Significant Quality Issues (by heuristic rules)\n")
                quality_text <- paste0(quality_text, "• Major data quality concerns may threaten validity\n")
                quality_text <- paste0(quality_text, "• Caution: Analysis may produce unreliable results\n")
                quality_text <- paste0(quality_text, "• Investigate root causes of quality problems\n")
                quality_text <- paste0(quality_text, "• Consider whether data can be salvaged or need re-collection\n")
                quality_text <- paste0(quality_text, "• Consult with senior investigator before proceeding\n")
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

            if (!is.null(clinical_issues_found) && length(clinical_issues_found) > 0) {
                quality_text <- paste0(quality_text, "• CLINICAL VALIDATION: Verify measurement units and clinical plausibility\n")
                quality_text <- paste0(quality_text, "• CLINICAL VALIDATION: Review data collection procedures\n")
            }
            
            if (n_total < 30) {
                quality_text <- paste0(quality_text, "• SAMPLE SIZE: Consider collecting additional data for robust analysis\n")
                quality_text <- paste0(quality_text, "• SAMPLE SIZE: Use appropriate methods for small sample sizes\n")
            }
            
            # Add validation warnings if present
            if (length(validation_results$warnings) > 0) {
                quality_text <- paste0(quality_text, "\nVALIDATION WARNINGS:\n")
                for (warning in validation_results$warnings) {
                    quality_text <- paste0(quality_text, sprintf("• %s\n", warning))
                }
            }
            
            # Add recommendations if present
            if (length(validation_results$recommendations) > 0) {
                quality_text <- paste0(quality_text, "\nADDITIONAL RECOMMENDATIONS:\n")
                for (rec in validation_results$recommendations) {
                    quality_text <- paste0(quality_text, sprintf("• %s\n", rec))
                }
            }
            
            # Add context-specific limitations section
            quality_text <- paste0(quality_text, "\n")
            quality_text <- paste0(quality_text, "⚠️  LIMITATIONS OF THIS ASSESSMENT:\n\n")

            limitations_added <- FALSE

            # Outlier detection limitations
            if (is_numeric && self$options$showOutliers) {
                if (n_complete < 10) {
                    quality_text <- paste0(quality_text, "• OUTLIERS (n=", n_complete, "): Informative only, NOT statistically robust.\n")
                    quality_text <- paste0(quality_text, "  Single-method flags shown for early QC; manually verify before taking action.\n")
                    limitations_added <- TRUE
                } else if (outliers_found > 0) {
                    outlier_transform_note <- if (exists("outlier_analysis") && !is.null(outlier_analysis$transform_applied)) {
                        if (outlier_analysis$transform_applied != "none") {
                            paste0(" on ", outlier_analysis$transform_applied, "-transformed scale")
                        } else {
                            ""
                        }
                    } else {
                        ""
                    }
                    quality_text <- paste0(quality_text, "• OUTLIERS: Consensus detection", outlier_transform_note, "; assumes approximate normality.\n")

                    if (exists("skewness") && abs(skewness) > 1 &&
                        (!exists("outlier_analysis") || is.null(outlier_analysis$transform_applied) || outlier_analysis$transform_applied == "none")) {
                        quality_text <- paste0(quality_text, "  WARNING: Severe skewness (", round(skewness, 2), ") without transform may cause Z-score false positives.\n")
                    }
                    limitations_added <- TRUE
                }
            }

            # Missingness limitations
            if (n_missing > 0) {
                quality_text <- paste0(quality_text, "• MISSINGNESS: Pattern tests are HEURISTIC")
                if (n_missing < 5 || n_complete < 5) {
                    quality_text <- paste0(quality_text, "; insufficient data (n_miss=", n_missing, ", n_complete=", n_complete, ") for runs test.\n")
                } else {
                    quality_text <- paste0(quality_text, "; cannot definitively prove MCAR vs MAR vs MNAR mechanisms.\n")
                }
                if (missing_pct > 20) {
                    quality_text <- paste0(quality_text, "  WARNING: High missingness (", round(missing_pct, 1), "%) may bias complete-case analysis; consider imputation.\n")
                }
                limitations_added <- TRUE
            }

            # Clinical checks limitations
            if (self$options$clinicalValidation && exists("clinical_issues_found") &&
                !is.null(clinical_issues_found) && length(clinical_issues_found) > 0) {
                quality_text <- paste0(quality_text, "• CLINICAL CHECKS: Hard-coded plausibility ranges may not suit all populations.\n")
                quality_text <- paste0(quality_text, "  May over-flag: pediatric, ICU, elite athletes, or diverse ethnic populations.\n")
                quality_text <- paste0(quality_text, "  Unit auto-detection is heuristic; override if incorrect. Manually verify flagged values.\n")
                limitations_added <- TRUE
            }

            # Quality score limitations (always shown)
            quality_text <- paste0(quality_text, "• QUALITY SCORE: Based on ARBITRARY thresholds and penalties, NOT externally validated.\n")
            quality_text <- paste0(quality_text, "  NOT suitable for regulatory submissions or as standalone quality metric.\n")
            quality_text <- paste0(quality_text, "  Use as initial screening tool only; review component breakdown for specific issues.\n")
            limitations_added <- TRUE

            # CV limitations
            if (is_numeric && self$options$showDistribution) {
                cv_min_mean <- self$options$cvMinMean
                quality_text <- paste0(quality_text, "• CV CALCULATION: Suppressed when |mean| < ", cv_min_mean, " to avoid instability.\n")
                quality_text <- paste0(quality_text, "  Use MAD or IQR for spread when CV not reported. CV only appropriate for ratio-scale data.\n")
                limitations_added <- TRUE
            }

            # General limitation footer
            if (limitations_added) {
                quality_text <- paste0(quality_text, "\n⚡ CRITICAL REMINDER: This is an automated SCREENING tool to identify potential issues.\n")
                quality_text <- paste0(quality_text, "   Final data quality decisions MUST incorporate:\n")
                quality_text <- paste0(quality_text, "   - Clinical/domain expertise for context\n")
                quality_text <- paste0(quality_text, "   - Manual verification of flagged values\n")
                quality_text <- paste0(quality_text, "   - Statistical judgment for analysis planning\n")
                quality_text <- paste0(quality_text, "   - Study-specific quality requirements\n")
            }

            quality_text <- paste0(quality_text, "\n═══════════════════════════════════════════════════════\n")
            quality_text <- paste0(quality_text, "Generated by ClinicoPath Data Quality Assessment Module\n")
            quality_text <- paste0(quality_text, sprintf("Assessment Date: %s\n", Sys.Date()))
            quality_text <- paste0(quality_text, "═══════════════════════════════════════════════════════")

            self$results$qualityText$setContent(quality_text)

            # ========== EDUCATIONAL PANELS ==========

            # Natural-Language Summary (for copying to reports)
            if (self$options$showSummary) {
                summary_html <- "<div style='font-family: Georgia, serif; line-height: 1.8; padding: 15px; background-color: #f9f9f9; border-left: 4px solid #2c5aa0;'>"
                summary_html <- paste0(summary_html, "<h3 style='color: #2c5aa0; margin-top: 0;'>Data Quality Summary</h3>")
                summary_html <- paste0(summary_html, "<p><strong>Variable:</strong> ", var_name, "</p>")
                summary_html <- paste0(summary_html, "<p><strong>Overall Quality Grade:</strong> ", quality_grade, " (", quality_score, "/100 by heuristic scoring)</p>")

                # Sample characteristics
                if (is_numeric) {
                    summary_html <- paste0(summary_html, sprintf("<p>This numeric variable contains <strong>%d observations</strong> with <strong>%.1f%% missing data</strong> (%d/%d cases). ", n_total, missing_pct, n_missing, n_total))
                } else {
                    summary_html <- paste0(summary_html, sprintf("<p>This categorical variable contains <strong>%d observations</strong> across <strong>%d unique categories</strong> with <strong>%.1f%% missing data</strong> (%d/%d cases). ", n_total, length(unique(variable[!is.na(variable)])), missing_pct, n_missing, n_total))
                }

                # Key findings
                if (outliers_found > 0) {
                    summary_html <- paste0(summary_html, sprintf("Consensus outlier detection identified <strong>%d potential outliers</strong> (%.1f%% of non-missing cases). ", outliers_found, (outliers_found/n_complete)*100))
                }

                if (!is.null(clinical_issues_found) && length(clinical_issues_found) > 0) {
                    summary_html <- paste0(summary_html, sprintf("Clinical plausibility checks flagged <strong>%d observations</strong> with values outside typical ranges. ", length(clinical_issues_found)))
                }

                summary_html <- paste0(summary_html, "</p>")

                # Quality interpretation
                summary_html <- paste0(summary_html, "<p><strong>Interpretation:</strong> ")
                if (quality_grade == "A") {
                    summary_html <- paste0(summary_html, "The data show <strong>excellent quality</strong> with minimal issues detected. The variable appears suitable for standard statistical analysis without major concerns.")
                } else if (quality_grade == "B") {
                    summary_html <- paste0(summary_html, "The data show <strong>good quality</strong> with minor issues that should be documented but do not prevent analysis. Review specific flagged observations and note any limitations in study methods.")
                } else if (quality_grade == "C") {
                    summary_html <- paste0(summary_html, "The data show <strong>quality concerns</strong> that may affect analysis validity. Careful review of specific issues is recommended, and sensitivity analyses should be performed to assess impact on study conclusions.")
                } else {
                    summary_html <- paste0(summary_html, "The data show <strong>significant quality issues</strong> that may threaten analysis validity. Major concerns include high missing data rates or numerous outliers/implausible values. Consider whether data cleaning or re-collection is necessary before proceeding with analysis.")
                }
                summary_html <- paste0(summary_html, "</p>")

                # Recommendations
                summary_html <- paste0(summary_html, "<p><strong>Recommendations:</strong> ")
                recommendations <- c()
                if (missing_pct > 15) recommendations <- c(recommendations, "investigate missing data mechanisms")
                if (outliers_found > 0) recommendations <- c(recommendations, "manually verify flagged outliers")
                if (!is.null(clinical_issues_found) && length(clinical_issues_found) > 0) recommendations <- c(recommendations, "verify clinical plausibility of flagged values")
                if (n_total < 30) recommendations <- c(recommendations, "consider collecting additional data")

                if (length(recommendations) > 0) {
                    summary_html <- paste0(summary_html, paste(recommendations, collapse = ", "), ".")
                } else {
                    summary_html <- paste0(summary_html, "No immediate actions required. Proceed with standard analysis protocols.")
                }
                summary_html <- paste0(summary_html, "</p>")

                summary_html <- paste0(summary_html, "<p style='font-size: 0.9em; color: #666; margin-top: 15px;'><em>Note: This assessment uses heuristic quality rules and should be combined with clinical/domain expertise for final data quality decisions.</em></p>")
                summary_html <- paste0(summary_html, "</div>")

                self$results$naturalSummary$setContent(summary_html)
            }

            # About This Analysis panel
            if (self$options$showAbout) {
                about_html <- "<div style='font-family: Arial, sans-serif; line-height: 1.6; padding: 15px; background-color: #f0f8ff; border-left: 4px solid #4682b4;'>"
                about_html <- paste0(about_html, "<h3 style='color: #4682b4; margin-top: 0;'>About Data Quality Assessment</h3>")

                about_html <- paste0(about_html, "<h4>Purpose</h4>")
                about_html <- paste0(about_html, "<p>This analysis performs comprehensive quality assessment for single variables to identify potential data issues before statistical analysis. It helps researchers detect missing data patterns, outliers, clinical implausibility, and other quality concerns that may affect study validity.</p>")

                about_html <- paste0(about_html, "<h4>Assessment Components</h4>")
                about_html <- paste0(about_html, "<ul>")
                about_html <- paste0(about_html, "<li><strong>Missing Data Analysis:</strong> Examines completeness, missing data patterns, and heuristic assessment of potential mechanisms (MCAR/MAR/MNAR) using runs test when sample size permits</li>")
                about_html <- paste0(about_html, "<li><strong>Outlier Detection:</strong> Uses consensus approach requiring agreement from ≥2 methods (Z-score |z|>3, IQR 1.5×rule, Modified Z-score MAD-based |z|>3.5) to minimize false positives</li>")
                about_html <- paste0(about_html, "<li><strong>Distribution Analysis:</strong> Provides descriptive statistics, normality assessment, and coefficient of variation for numeric variables</li>")
                about_html <- paste0(about_html, "<li><strong>Clinical Validation:</strong> Applies hard-coded plausibility ranges for common clinical variables (age, vital signs, lab values) with configurable unit systems</li>")
                about_html <- paste0(about_html, "<li><strong>Quality Scoring:</strong> Generates heuristic composite score (0-100) based on completeness, outlier prevalence, sample size, and variability</li>")
                about_html <- paste0(about_html, "</ul>")

                about_html <- paste0(about_html, "<h4>Quality Grade Interpretation</h4>")
                about_html <- paste0(about_html, "<ul>")
                about_html <- paste0(about_html, "<li><strong>Grade A (85-100):</strong> Excellent quality - minimal issues, suitable for standard analysis</li>")
                about_html <- paste0(about_html, "<li><strong>Grade B (70-84):</strong> Good quality - minor issues requiring documentation but analysis can proceed</li>")
                about_html <- paste0(about_html, "<li><strong>Grade C (50-69):</strong> Quality concerns - significant issues requiring review and sensitivity analyses</li>")
                about_html <- paste0(about_html, "<li><strong>Grade D (<50):</strong> Poor quality - major validity threats, consider data cleaning or re-collection</li>")
                about_html <- paste0(about_html, "</ul>")

                about_html <- paste0(about_html, "<h4>Advanced Options</h4>")
                about_html <- paste0(about_html, "<ul>")
                about_html <- paste0(about_html, "<li><strong>Outlier Transformation:</strong> Apply log or square root transformations before outlier detection for right-skewed distributions (common in lab values)</li>")
                about_html <- paste0(about_html, "<li><strong>MCAR Test:</strong> Perform Little's MCAR test if naniar package available for formal statistical test of missing completely at random assumption</li>")
                about_html <- paste0(about_html, "<li><strong>Rare Category Threshold:</strong> Flag categories occurring in <X% of observations (important for chi-squared test assumptions and modeling stability)</li>")
                about_html <- paste0(about_html, "</ul>")

                about_html <- paste0(about_html, "<p style='font-size: 0.9em; color: #666; margin-top: 15px;'><em>For detailed methodology and validation studies, see ClinicoPath module documentation at <a href='https://www.serdarbalci.com/ClinicoPathDescriptives/' target='_blank'>https://www.serdarbalci.com/ClinicoPathDescriptives/</a></em></p>")
                about_html <- paste0(about_html, "</div>")

                self$results$aboutAnalysis$setContent(about_html)
            }

            # Caveats & Assumptions panel
            if (self$options$showCaveats) {
                caveats_html <- "<div style='font-family: Arial, sans-serif; line-height: 1.6; padding: 15px; background-color: #fff8dc; border-left: 4px solid #ffa500;'>"
                caveats_html <- paste0(caveats_html, "<h3 style='color: #d2691e; margin-top: 0;'>⚠️ Important Caveats & Assumptions</h3>")

                caveats_html <- paste0(caveats_html, "<h4>Heuristic-Based Assessment</h4>")
                caveats_html <- paste0(caveats_html, "<ul>")
                caveats_html <- paste0(caveats_html, "<li><strong>Quality scores and grades are NOT externally validated:</strong> Thresholds and penalty weights are based on statistical rules of thumb, not empirical validation studies</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Not suitable for regulatory submissions:</strong> This is a screening tool for research workflows, not a validated quality metric for FDA/EMA submissions</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Context matters:</strong> Quality thresholds appropriate for clinical trials may differ from observational studies, pilot studies, or exploratory analyses</li>")
                caveats_html <- paste0(caveats_html, "</ul>")

                caveats_html <- paste0(caveats_html, "<h4>Outlier Detection Limitations</h4>")
                caveats_html <- paste0(caveats_html, "<ul>")
                caveats_html <- paste0(caveats_html, "<li><strong>Assumes approximate normality:</strong> Z-score and MAD methods work best for symmetric distributions; severely skewed data may produce false positives</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Transformation trade-offs:</strong> Log/sqrt transforms reduce false positives in skewed data but complicate interpretation of flagged values on original scale</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Small sample sensitivity:</strong> With n<30, outlier flags are informative only; consensus requirement is relaxed to single-method for very small samples (n<10)</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>True outliers vs errors:</strong> Statistical outliers may represent valid extreme values (e.g., elite athletes, rare diseases); clinical judgment required</li>")
                caveats_html <- paste0(caveats_html, "</ul>")

                caveats_html <- paste0(caveats_html, "<h4>Missing Data Assessment Limitations</h4>")
                caveats_html <- paste0(caveats_html, "<ul>")
                caveats_html <- paste0(caveats_html, "<li><strong>Cannot definitively prove MCAR/MAR/MNAR:</strong> Runs test provides heuristic pattern assessment but formal distinction requires specialized methods (e.g., sensitivity analyses, pattern-mixture models)</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Single-variable limitation:</strong> Missing data mechanisms often involve relationships between variables; multivariate approaches (Little's MCAR test with multiple variables) provide stronger evidence</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Informative missingness:</strong> Even low missing percentages can bias results if missingness is related to outcome (MNAR)</li>")
                caveats_html <- paste0(caveats_html, "</ul>")

                caveats_html <- paste0(caveats_html, "<h4>Clinical Validation Limitations</h4>")
                caveats_html <- paste0(caveats_html, "<ul>")
                caveats_html <- paste0(caveats_html, "<li><strong>Hard-coded reference ranges:</strong> Plausibility bounds are based on general population norms and may not suit all contexts (pediatric, ICU, elite athletes, diverse ethnic populations)</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Unit auto-detection is heuristic:</strong> May misclassify units in edge cases (e.g., height in inches vs cm); manually verify unit system selection</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Variable name matching:</strong> Clinical checks use pattern matching on variable names (e.g., 'age', 'glucose', 'systolic'); may miss or misclassify non-standard naming</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Context-specific ranges:</strong> Normal ranges vary by measurement method, population demographics, and clinical context; verify against study-specific protocols</li>")
                caveats_html <- paste0(caveats_html, "</ul>")

                caveats_html <- paste0(caveats_html, "<h4>Statistical Assumptions</h4>")
                caveats_html <- paste0(caveats_html, "<ul>")
                caveats_html <- paste0(caveats_html, "<li><strong>Independence assumption:</strong> Outlier and normality tests assume independent observations; may be violated in clustered/longitudinal data</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>CV calculation:</strong> Coefficient of variation only appropriate for ratio-scale data with meaningful zero; suppressed when |mean| < threshold to avoid instability</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Normality tests:</strong> Shapiro-Wilk test is sensitive to sample size (low power for n<30, overpowered for large n); use Q-Q plots and domain knowledge</li>")
                caveats_html <- paste0(caveats_html, "</ul>")

                caveats_html <- paste0(caveats_html, "<h4>Recommended Workflow</h4>")
                caveats_html <- paste0(caveats_html, "<p style='background-color: #fff; padding: 10px; border-left: 3px solid #ffa500;'>")
                caveats_html <- paste0(caveats_html, "<strong>Step 1:</strong> Use this tool for initial automated screening<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 2:</strong> Manually verify all flagged observations with clinical/domain expertise<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 3:</strong> Investigate root causes (data entry errors, measurement issues, true biological variation)<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 4:</strong> Document all quality decisions and cleaning actions with justification<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 5:</strong> Perform sensitivity analyses comparing results with/without flagged observations<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 6:</strong> Report quality assessment and handling in study methods section")
                caveats_html <- paste0(caveats_html, "</p>")

                caveats_html <- paste0(caveats_html, "<p style='font-size: 0.9em; color: #d2691e; margin-top: 15px; font-weight: bold;'>")
                caveats_html <- paste0(caveats_html, "⚡ CRITICAL: Automated quality assessment is a starting point, not a substitute for statistical and clinical judgment. Always combine algorithmic screening with expert review before making data cleaning decisions.")
                caveats_html <- paste0(caveats_html, "</p>")

                caveats_html <- paste0(caveats_html, "</div>")

                self$results$caveatsAssumptions$setContent(caveats_html)
            }
        }
    )
)
