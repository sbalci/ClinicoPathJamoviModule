# Enhanced data quality assessment for clinical research
# Provides comprehensive evaluation of data completeness, accuracy, and patterns

checkdataClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "checkdataClass",
    inherit = checkdataBase,
    private = list(

        # Track the outlier-detection transform applied in the most recent
        # .populateOutlierAnalysis call, so .run's limitations text can annotate
        # the scale (the helper-local `outlier_analysis` is not visible in .run).
        .outlier_transform = "none",

        # TRUE once outlier detection has actually been computed in the current
        # run. Distinguishes "no value was flagged" from "outlier screening was
        # never run" (option unticked, non-numeric variable, or n < 3). The
        # quality summary and the scoring breakdown must not conflate the two:
        # .populateOutlierAnalysis returns 0 for both.
        .outliers_assessed = FALSE,

        # Set alongside .outliers_assessed by .populateOutlierAnalysis.
        #
        # .outliers_informative_only is TRUE for 3 <= n_complete < 10, where
        # .advancedOutlierDetection relaxes consensus to a SINGLE method and
        # labels the result "not statistically robust". Those flags must not feed
        # the quality grade or the outlier-rate warnings: at n = 9 one IQR flag is
        # an 11.1% outlier rate, which cost 20 points on top of the 30-point
        # small-sample penalty and produced a warning telling the user to review
        # each outlier - all from a flag the analysis had itself declared
        # non-robust.
        #
        # .outlier_n_methods is 2 whenever the MAD method is unavailable
        # (n_complete <= 3, or MAD == 0 because half the values are identical),
        # so no message may hard-code "of the 3 methods".
        .outliers_informative_only = FALSE,
        .outlier_n_methods = 3,

        # Generate interpretation for missing data
        .interpretMissing = function(missing_pct) {
            if (missing_pct == 0) {
                return(.("Excellent - Complete data"))
            } else if (missing_pct < 5) {
                return(.("Good - Minimal missing data"))
            } else if (missing_pct < 15) {
                return(.("Acceptable - Some missing data"))
            } else if (missing_pct < 30) {
                return(.("Concerning - Substantial missing data"))
            } else {
                return(.("Poor - Extensive missing data"))
            }
        },
        
        # Assess skewness interpretation
        .interpretSkewness = function(skewness) {
            abs_skew <- abs(skewness)
            if (abs_skew < 0.5) {
                return(.("Approximately symmetric"))
            } else if (abs_skew < 1) {
                return(.("Moderately skewed"))
            } else {
                return(.("Highly skewed"))
            }
        },

        # Moment coefficient of skewness (g1) using consistent population
        # moments; avoids mixing a population 3rd moment with the sample SD.
        .computeSkewness = function(x) {
            n <- length(x)
            if (n < 3) return(0)
            mu <- mean(x)
            m2 <- mean((x - mu)^2)
            m3 <- mean((x - mu)^3)
            if (m2 <= 0) return(0)
            m3 / m2^1.5
        },
        
        # Magnitude label for a point that has ALREADY been flagged as an
        # outlier. The argument is a robust deviation in approximate SD units:
        # the MAD-based modified Z-score where it is available, otherwise the
        # ordinary Z-score.
        #
        # The bin below |score| = 2 must NOT be a magnitude class. An earlier
        # version returned "Not an outlier" there, which was wrong for a row that
        # had reached the outliers table; the fix collapsed the floor into "Mild",
        # which is wrong in the other direction. Below n = 10 consensus is relaxed
        # to a single method, so x = c(1,1,1,1,1,1,2,10) reaches this table twice:
        # MAD is 0 (no modified Z), the IQR fence alone flags the value 2, and its
        # ordinary Z is -0.079. Labelling a point 0.08 SD from the centre "Mild"
        # asserts a magnitude the statistic does not show. The floor now names
        # what was actually measured, and the caller states which statistic was
        # ranked and how many methods fired.
        .outlierSeverity = function(score) {
            abs_s <- abs(score)
            if (!is.finite(abs_s)) {
                return(.("Undetermined"))
            } else if (abs_s > 4) {
                return(.("Extreme"))
            } else if (abs_s > 3.5) {
                return(.("Very High"))
            } else if (abs_s > 3) {
                return(.("High"))
            } else if (abs_s > 2.5) {
                return(.("Moderate"))
            } else if (abs_s > 2) {
                return(.("Mild"))
            } else {
                return(.("Below magnitude thresholds"))
            }
        },

        # Split a column name into lower-case word tokens, honouring separators
        # (space, underscore, dot, dash, digits) AND camelCase boundaries, so
        # "AgeAtDiagnosis", "age_years" and "Age (years)" all yield the token
        # "age" while "TStage" and "Percentage" do not.
        .nameTokens = function(var_name) {
            if (length(var_name) != 1 || is.na(var_name))
                return(character(0))
            s <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", var_name)
            s <- gsub("([A-Z]+)([A-Z][a-z])", "\\1 \\2", s)
            tokens <- strsplit(tolower(s), "[^a-z0-9]+")[[1]]
            tokens[nzchar(tokens)]
        },

        # TRUE when any whole word of the column name is one of `words`.
        #
        # The two name-matching policies in this file were wrong in opposite
        # directions. The clinical checks matched unanchored substrings:
        # grepl("creatinine|cr", name) fires on Necrosis, NecrosisPercent,
        # Cribriform, Crush, Screening and Description, so a NecrosisPercent
        # column holding 0-100 was reported as "Creatinine outside 30-1000
        # umol/L" - a false flag that also cost up to 20 points of the headline
        # quality grade. grepl("age", name) fires on TStage and Percentage, and
        # grepl("hemoglobin|hgb|hb", name) on HBsAg and HBV. .validateData used
        # the opposite extreme, an exact all-lowercase match that never fired on
        # a real column name ("Age"). Whole-word, case-insensitive matching is
        # now the single policy both paths share, so they cannot drift again.
        .nameMatches = function(var_name, words) {
            if (length(var_name) != 1 || is.na(var_name))
                return(FALSE)
            # Whole-name form with separators removed, so a name that
            # camelCase-splits into fragments ("SCr" -> "s", "cr") still matches
            # its own compact form ("scr"). This is still a whole-NAME match and
            # never a substring match, so "NecrosisPercent" cannot reach
            # "creatinine" and "TStage" cannot reach "age".
            compact <- tolower(gsub("[^A-Za-z0-9]", "", var_name))
            tokens <- private$.nameTokens(var_name)
            # A derived quantity carries the raw measurement's name but not its
            # units, so the raw reference range must not be applied to it.
            # "HbA1c" tokenises to [hb, a1c] and hit the haemoglobin rules: in
            # IFCC units (mmol/mol, roughly 20-130) any(> 25) picked the SI
            # branch and any(< 30) then fired on every value in the 20-29 band,
            # reporting "Hemoglobin outside 30-200 g/L". "CreatinineClearance"
            # tokenises to [creatinine, clearance] and hit the creatinine rules,
            # reporting an ordinary mL/min clearance as "Creatinine outside
            # 30-1000 umol/L". Both are common column names and each false flag
            # also cost 5 points of the headline grade. The compact form is
            # tested too, because "HbA1C" splits to [hb, a1, c] and never yields
            # an "a1c" token.
            if (any(tokens %in% private$.nameDisqualifiers) ||
                compact %in% private$.nameDisqualifiers)
                return(FALSE)
            if (any(tokens %in% words))
                return(TRUE)
            isTRUE(compact %in% words)
        },

        # Whole words that mark a column as a derived / relative quantity rather
        # than the raw measurement the plausibility ranges are written for.
        .nameDisqualifiers = c("a1c", "hba1c", "clearance", "ratio", "percent",
                               "index", "score", "change", "delta"),
        
        # Enhanced data validation with comprehensive error checking
        .validateData = function(variable, var_name) {
            validation_results <- list(
                is_valid = TRUE,
                error_messages = character(),
                warnings = character(),
                recommendations = character()
            )
            
            # Check for completely empty variable. This also covers an empty
            # dataset and a row filter that excludes every case: self$data[[var]]
            # then has length 0. It is the single error exit of the analysis -
            # .run() no longer carries its own nrow(self$data) == 0 test, whose
            # earlier return used to make this branch unreachable.
            if (length(variable) == 0) {
                validation_results$is_valid <- FALSE
                validation_results$error_messages <- c(validation_results$error_messages,
                    .fmt(.("Variable '{var}' contains no observations, so no quality assessment can be computed. This happens when the dataset has no rows, or when a row filter excludes every case. Load data or relax the filter, then re-run."),
                         var = var_name))
                return(validation_results)
            }
            
            # Check for all missing data
            if (all(is.na(variable))) {
                validation_results$warnings <- c(validation_results$warnings,
                    .fmt(.("Variable {var} contains only missing values"), var = var_name))
                validation_results$recommendations <- c(validation_results$recommendations,
                    .("Consider investigating data collection procedures"))
            }
            
            # Check for single value (no variability)
            if (length(unique(na.omit(variable))) == 1) {
                validation_results$warnings <- c(validation_results$warnings,
                    .fmt(.("Variable {var} contains only one unique value"), var = var_name))
                validation_results$recommendations <- c(validation_results$recommendations,
                    .("Verify if constant value reflects true data structure"))
            }
            
            # Check for very small sample size
            complete_n <- sum(!is.na(variable))
            if (complete_n < 10) {
                validation_results$warnings <- c(validation_results$warnings,
                    .fmt(.("Small sample size (n = {n}) may limit reliability"), n = complete_n))
                validation_results$recommendations <- c(validation_results$recommendations,
                    .("Consider collecting additional data or interpreting results cautiously"))
            }
            
            # Numeric-specific validations
            if (is.numeric(variable)) {
                clean_var <- variable[!is.na(variable)]
                
                # Infinite values are counted here and treated as missing by
                # every summary (.run() blanks them right after validation).
                # They must leave THIS function's checks too: max() - min() is
                # Inf and mean() is Inf or NaN, and `Inf > NaN` is NA, which
                # aborted the run at the range test below.
                n_infinite <- sum(is.infinite(clean_var))
                if (n_infinite > 0) {
                    validation_results$warnings <- c(validation_results$warnings,
                        .fmt(.("{n} infinite values (Inf or -Inf) detected in numeric data; they are treated as missing in every summary"),
                             n = n_infinite))
                    validation_results$recommendations <- c(validation_results$recommendations,
                        .("Review data processing procedures for infinite value generation"))
                    clean_var <- clean_var[is.finite(clean_var)]
                }
                
                # Check for extreme range (possible data entry errors)
                if (length(clean_var) > 1) {
                    data_range <- max(clean_var) - min(clean_var)
                    mean_val <- mean(clean_var)
                    if (data_range > 1000 * abs(mean_val)) {
                        validation_results$warnings <- c(validation_results$warnings,
                            .("Extremely wide data range detected - possible data entry errors"))
                        validation_results$recommendations <- c(validation_results$recommendations,
                            .("Verify extreme values for data entry accuracy"))
                    }
                }
                
                # Check for negative values in contexts where they shouldn't exist
                if (any(clean_var < 0) &&
                    private$.nameMatches(var_name, c("age", "weight", "height", "time", "duration", "count"))) {
                    validation_results$warnings <- c(validation_results$warnings,
                        .fmt(.("Negative values detected in {var} which should typically be positive"), var = var_name))
                    validation_results$recommendations <- c(validation_results$recommendations,
                        .("Review negative values for biological/clinical plausibility"))
                }
            }
            
            # Categorical-specific validations
            if (is.factor(variable) || is.character(variable)) {
                clean_var <- variable[!is.na(variable)]
                
                # Check for high cardinality (may indicate ID variables)
                unique_count <- length(unique(clean_var))
                if (unique_count > 0.8 * length(clean_var)) {
                    validation_results$warnings <- c(validation_results$warnings,
                        .("Very high cardinality - variable may be identifier rather than categorical"))
                    validation_results$recommendations <- c(validation_results$recommendations,
                        .("Verify if variable should be treated as categorical for analysis"))
                }
                
                # Check for inconsistent category encoding
                if (is.character(variable)) {
                    # Look for common inconsistencies
                    unique_vals <- unique(clean_var)
                    if (any(grepl("^(male|female)$", unique_vals, ignore.case = TRUE)) &&
                        any(grepl("^(m|f)$", unique_vals, ignore.case = TRUE))) {
                        validation_results$warnings <- c(validation_results$warnings,
                            .("Inconsistent category encoding detected (e.g., 'Male' vs 'M')"))
                        validation_results$recommendations <- c(validation_results$recommendations,
                            .("Standardize category labels before analysis"))
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

            # Apply transformation if requested (for right-skewed distributions).
            #
            # A requested transform that cannot be applied MUST leave
            # transform_applied as the literal "none". Three downstream consumers
            # test `transform_applied != "none"`, so the previous sentence-valued
            # "none (negative values present)" sent all three down the
            # transform-was-applied path and produced text such as
            # "High (2/3 methods on none (negative values present) scale)" and a
            # Method Summary row whose Threshold cell held that sentence. The
            # reason now travels separately and is shown once, as a table note.
            # The log guard is `> 0`, so a zero blocks it too - the old wording
            # claimed negative values in percentages and counts that had none.
            transform_type <- self$options$outlierTransform
            transform_applied <- "none"
            transform_skipped <- NULL
            if (transform_type == "log") {
                if (all(clean_var > 0)) {
                    clean_var <- log(clean_var)
                    transform_applied <- "log"
                } else {
                    # The square root is only an alternative when the log was
                    # blocked by zeros ALONE: the sqrt branch below requires
                    # all(clean_var >= 0), so with a negative value present it is
                    # skipped for exactly the same data and following the advice
                    # would earn a second "not applied" note.
                    log_alternative <- if (any(clean_var < 0)) {
                        .("Negative values are present, so the square-root transform is unavailable for this variable as well; add a constant offset before the analysis if a transform is needed.")
                    } else {
                        .("Add a constant offset before the analysis, or choose the square-root transform, if a transform is needed.")
                    }
                    transform_skipped <- paste(
                        .fmt(.("Log transform requested but not applied: {nbad} of {ntotal} complete values are zero or negative, where the logarithm is undefined. Outlier detection ran on the raw values instead."),
                             nbad = sum(clean_var <= 0), ntotal = length(clean_var)),
                        log_alternative)
                }
            } else if (transform_type == "sqrt") {
                if (all(clean_var >= 0)) {
                    clean_var <- sqrt(clean_var)
                    transform_applied <- "sqrt"
                } else {
                    transform_skipped <- .fmt(
                        .("Square-root transform requested but not applied: {nbad} of {ntotal} complete values are negative, where the square root is undefined. Outlier detection ran on the raw values instead."),
                        nbad = sum(clean_var < 0), ntotal = length(clean_var))
                }
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
            modified_z_all <- NULL
            if (length(clean_var) > 3) {
                # Iglewicz & Hoaglin (1993) modified Z-score, as in the NIST
                # e-Handbook 1.3.5.17:  M_i = 0.6745 (x_i - median) / MAD_raw,
                # where MAD_raw = median(|x_i - median|).
                #
                # The scale correction must be applied ONCE. R's mad() already
                # multiplies by constant = 1.4826 to make MAD a consistent
                # estimator of sigma, and 0.6745 = 1/1.4826, so the previous
                # `0.6745 * (x - median) / mad(x, constant = 1.4826)` divided by
                # the factor twice. Every modified Z came out 1.4826x too small,
                # which turned the >3.5 cut-off into an effective >5.19 and made
                # the method labelled "most robust" the least sensitive of the
                # three. Measured on 100 lab values plus 3 contaminants sitting
                # 3.5 SD out, over 300 replicates: the correct formula flags
                # 325/900 contaminants, the old one flagged 0/900, and 88 were
                # lost from the consensus outlier table entirely.
                mad_val <- mad(clean_var, constant = 1.4826)  # consistent estimate of sigma
                if (mad_val > 0) {
                    modified_z <- (clean_var - median(clean_var)) / mad_val
                    modified_z_all <- modified_z  # full vector, for severity ranking
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
            # For larger samples, require consensus (>=2 methods)
            if (is_small_sample) {
                # Informative-only: show any point flagged by at least 1 method
                consensus_outliers <- which(detection_count >= 1)
                is_informative_only <- TRUE
                consensus_note <- .("INFORMATIVE ONLY (n<10): Single-method flags shown, not statistically robust")
            } else {
                # Standard consensus: require >=2 methods
                consensus_outliers <- which(detection_count >= 2)
                is_informative_only <- FALSE
                consensus_note <- .("Consensus outliers (>=2 methods)")
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
                transform_skipped = transform_skipped,
                modified_z_scores = modified_z_all,  # NULL when MAD unavailable
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

            # Little's MCAR test is inherently MULTIVARIATE: it compares the
            # observed-data means across missingness patterns using the other
            # variables. With a single variable there is nothing to compare, so
            # the test is undefined here - not merely unimplemented.
            #
            # This block used to be unreachable: it was guarded on
            # `!is.null(data_context)`, and both call sites pass only `variable`,
            # so data_context was always NULL. Ticking "MCAR statistical test"
            # therefore did nothing at all, while the option's own description
            # promised "a formal test vs. heuristic assessment". Say plainly what
            # is and is not available instead of silently ignoring the request.
            if (isTRUE(self$options$mcarTest)) {
                patterns$mcar_not_applicable <- .("Little's MCAR test is a multivariate test and cannot be computed for a single variable - it compares means across missingness patterns using the other variables in the dataset. The runs and dropout results below are heuristics about WHERE the missing values sit, not a test of the missingness mechanism. To test MCAR formally, run naniar::mcar_test() on the full dataset.")
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
                                patterns$clustering <- .fmt(
                                    .("HEURISTIC: Missing data appears clustered (runs test p={p}, {runs} vs {expected} expected) possible systematic cause"),
                                    p = sprintf("%.3f", p_runs), runs = n_runs, expected = sprintf("%.1f", expected_runs))
                            } else {
                                patterns$alternating <- .fmt(
                                    .("HEURISTIC: Missing data alternates (runs test p={p}, {runs} vs {expected} expected) check data collection pattern"),
                                    p = sprintf("%.3f", p_runs), runs = n_runs, expected = sprintf("%.1f", expected_runs))
                            }
                        } else {
                            patterns$random <- .fmt(
                                .("HEURISTIC: Missing pattern consistent with randomness (runs test p={p})"),
                                p = sprintf("%.3f", p_runs))
                        }
                    } else {
                        patterns$random_note <- .("HEURISTIC: Runs test variance too small for reliable inference")
                    }
                } else {
                    patterns$insufficient <- .fmt(
                        .("HEURISTIC: Insufficient data (n_miss={nmiss}, n_complete={ncomp}) for runs test (need >=5 each)"),
                        nmiss = n_missing, ncomp = n_complete)
                }
            }

            # Pattern 2: HEURISTIC monotone missing (dropout pattern)
            if (n_missing > n_total * 0.1) {
                # Check if missing data concentrates at end (dropout)
                last_quarter_start <- round(n_total * 0.75)
                missing_in_last_quarter <- sum(missing_indices > last_quarter_start)
                dropout_prop <- missing_in_last_quarter / n_missing

                # Reference value for the comparison below: the share of ROWS
                # that lie after last_quarter_start (0.25 for n = 120, where
                # round(90) leaves rows 91-120). If missingness were unrelated to
                # row position that is the expected share of missing values
                # falling there. The previous code tested the Wilson lower bound
                # against 0.5 - twice the null - so 45% of missing in the last
                # quarter, an ~1.8x enrichment, fired neither branch; and the
                # printed CI invited comparison against a 50% that was never the
                # right reference.
                null_prop <- (n_total - last_quarter_start) / n_total

                # 95% CI for dropout proportion (Wilson score interval)
                if (n_missing > 0 && null_prop > 0 && null_prop < 1) {
                    p_hat <- dropout_prop
                    z <- 1.96
                    denom <- 1 + z^2 / n_missing
                    center <- (p_hat + z^2 / (2 * n_missing)) / denom
                    margin <- z * sqrt(p_hat * (1 - p_hat) / n_missing + z^2 / (4 * n_missing^2)) / denom
                    ci_low <- max(0, center - margin)
                    ci_high <- min(1, center + margin)

                    if (ci_low > null_prop) {
                        patterns$dropout <- .fmt(
                            .("HEURISTIC: Likely dropout pattern ({pct}% of missing values fall in the last quarter of rows, 95% CI: {lo}%-{hi}%; {expected}% expected if missingness were unrelated to row position)"),
                            pct = sprintf("%.1f", dropout_prop * 100), lo = sprintf("%.1f", ci_low * 100),
                            hi = sprintf("%.1f", ci_high * 100), expected = sprintf("%.1f", null_prop * 100))
                    } else if (dropout_prop > 1.5 * null_prop) {
                        patterns$possible_dropout <- .fmt(
                            .("HEURISTIC: Possible dropout pattern ({pct}% of missing values fall in the last quarter of rows, 95% CI: {lo}%-{hi}%; {expected}% expected if missingness were unrelated to row position) - the interval includes the expected share"),
                            pct = sprintf("%.1f", dropout_prop * 100), lo = sprintf("%.1f", ci_low * 100),
                            hi = sprintf("%.1f", ci_high * 100), expected = sprintf("%.1f", null_prop * 100))
                    }
                }
            }

            # Pattern 3: Missing data percentage thresholds with context
            if (missing_pct > 50) {
                patterns$severe <- .fmt(
                    .("Severe missing data ({pct}%) - major quality concern; analysis may be biased"),
                    pct = sprintf("%.1f", missing_pct))
            } else if (missing_pct > 20) {
                patterns$substantial <- .fmt(
                    .("Substantial missing data ({pct}%) - investigate MCAR/MAR/MNAR mechanisms"),
                    pct = sprintf("%.1f", missing_pct))
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
                return(list(quality_issues = .("All values missing")))
            }
            
            category_analysis <- list()
            
            # Category frequency analysis. Drop zero-count entries so unused
            # factor levels do not force a false "severe imbalance" (min_freq=0)
            # or get listed as rare categories.
            freq_table <- table(clean_var)
            freq_table <- freq_table[freq_table > 0]
            n_categories <- length(freq_table)
            n_total <- length(clean_var)
            
            # Category balance assessment
            if (n_categories > 1) {
                min_freq <- min(freq_table)
                max_freq <- max(freq_table)
                balance_ratio <- min_freq / max_freq
                
                if (balance_ratio < 0.1) {
                    category_analysis$imbalance <- .("Severe category imbalance detected")
                } else if (balance_ratio < 0.3) {
                    category_analysis$moderate_imbalance <- .("Moderate category imbalance")
                } else {
                    category_analysis$balanced <- .("Categories reasonably balanced")
                }
                
                # Rare category detection, on the same percentage rule the
                # Distribution table uses, so the two tables cannot disagree.
                rare_threshold_pct <- self$options$rareCategoryThreshold
                rare_categories <- names(freq_table)[freq_table < (rare_threshold_pct / 100) * n_total]
                if (length(rare_categories) > 0) {
                    category_analysis$rare_categories <- .fmt(
                        .("Rare categories below {pct}% of complete cases: {cats}"),
                        pct = sprintf("%.1f", rare_threshold_pct), cats = paste(rare_categories, collapse = ", "))
                }
            }
            
            # High cardinality check
            cardinality_ratio <- n_categories / n_total
            if (cardinality_ratio > 0.8) {
                category_analysis$high_cardinality <- .("Very high cardinality - may be identifier variable")
            } else if (cardinality_ratio > 0.5) {
                category_analysis$moderate_cardinality <- .("High cardinality - verify categorical nature")
            }
            
            # Category naming consistency (for character variables)
            if (is.character(variable)) {
                unique_vals <- unique(clean_var)
                
                # Check for case inconsistencies
                lower_vals <- tolower(unique_vals)
                if (length(unique(lower_vals)) < length(unique_vals)) {
                    category_analysis$case_inconsistency <- .("Case inconsistencies detected in categories")
                }
                
                # Check for leading/trailing spaces
                trimmed_vals <- trimws(unique_vals)
                if (any(trimmed_vals != unique_vals)) {
                    category_analysis$whitespace_issues <- .("Leading/trailing spaces detected in categories")
                }
            }
            
            return(category_analysis)
        },
        
        # Clinical context validation
        # Units are inferred from the value range: kg, cm or m, SI or conventional lab units.
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

            # Which rule set applies is decided by WHOLE WORDS of the column
            # name (see .nameMatches). Unanchored substring matching used to fire
            # these clinical rules on unrelated pathology columns - "age" on
            # TStage and Percentage, "cr" on Necrosis and Cribriform, "hb" on
            # HBsAg - producing a false plausibility flag, a Data Patterns row,
            # a warning notice and a deduction from the headline quality grade.
            # Two-letter abbreviations that cannot be disambiguated from common
            # pathology terms are deliberately absent from these word lists: a
            # missed check is silent, a wrong check is a false alarm on a
            # patient's data.

            # Age-specific validations (unit-agnostic)
            if (private$.nameMatches(var_name, c("age"))) {
                if (any(clean_var < 0)) {
                    clinical_issues$negative_age <- .("PLAUSIBILITY CHECK: Negative age values detected (biologically impossible)")
                }
                if (any(clean_var > 120)) {
                    clinical_issues$extreme_age <- .("PLAUSIBILITY CHECK: Age >120 years detected (threshold: 120) - verify data accuracy")
                }
                if (any(clean_var < 1 & clean_var > 0)) {
                    clinical_issues$fractional_age <- .("PLAUSIBILITY CHECK: Fractional age <1 detected - verify units (years vs months)")
                }
            }

            # Weight is checked in kilograms.
            if (private$.nameMatches(var_name, c("weight", "bodyweight"))) {
                if (any(clean_var < 2)) {
                    clinical_issues$low_weight <- .("PLAUSIBILITY CHECK: Weight <2 kg detected (assumed kg) - verify units or data entry")
                }
                if (any(clean_var > 200)) {
                    clinical_issues$high_weight <- .("PLAUSIBILITY CHECK: Weight >200 kg detected (assumed kg, threshold: 200) - verify accuracy")
                }
            }

            # Height is centimetres or metres; the value range decides which.
            if (private$.nameMatches(var_name, c("height", "bodyheight"))) {
                if (max(clean_var, na.rm = TRUE) > 10) {
                    if (any(clean_var < 50) || any(clean_var > 250)) {
                        clinical_issues$implausible_height <- .("PLAUSIBILITY CHECK: Height outside 50-250 cm range (assumed cm) - verify units")
                    }
                } else if (any(clean_var < 0.5) || any(clean_var > 2.5)) {
                    clinical_issues$implausible_height <- .("PLAUSIBILITY CHECK: Height outside 0.5-2.5 m range (assumed m) - verify units")
                }
            }

            # Laboratory value ranges (SI or conventional units, inferred from the value range)
            if (private$.nameMatches(var_name, c("hemoglobin", "haemoglobin", "hgb", "hb"))) {
                # g/dL is common in US, g/L in SI (multiply by 10)
                # Most data will be in g/dL range (3-20), g/L would be 30-200
                if (any(clean_var > 25)) {
                    # Likely g/L
                    if (any(clean_var < 30) || any(clean_var > 200)) {
                        clinical_issues$hemoglobin_range <- .("PLAUSIBILITY CHECK: Hemoglobin outside 30-200 g/L range (assumed SI units, threshold: 30-200) - verify units")
                    }
                } else {
                    # Likely g/dL
                    if (any(clean_var < 3) || any(clean_var > 20)) {
                        clinical_issues$hemoglobin_range <- .("PLAUSIBILITY CHECK: Hemoglobin outside 3-20 g/dL range (assumed traditional units, threshold: 3-20) - verify accuracy")
                    }
                }
            }

            # "cr" alone is excluded: in oncology and pathology datasets CR
            # commonly means complete response, not creatinine.
            if (private$.nameMatches(var_name, c("creatinine", "creat", "scr"))) {
                # mg/dL (US) typically 0.3-10, umol/L (SI) typically 30-1000
                if (any(clean_var > 20)) {
                    # Likely umol/L
                    if (any(clean_var < 30) || any(clean_var > 1000)) {
                        clinical_issues$creatinine_range <- .fmt(.("PLAUSIBILITY CHECK: Creatinine outside 30-1000 {mu}mol/L range (assumed SI units, threshold: 30-1000) - verify units"), mu = "\u{B5}")
                    }
                } else {
                    # Likely mg/dL
                    if (any(clean_var < 0.3) || any(clean_var > 10)) {
                        clinical_issues$creatinine_range <- .("PLAUSIBILITY CHECK: Creatinine outside 0.3-10 mg/dL range (assumed traditional units, threshold: 0.3-10) - verify accuracy")
                    }
                }
            }

            return(clinical_issues)
        },

        # Initialize function for setup tasks
        .init = function() {
            # Set initial visibility states
            # This runs once when the analysis is created

            # Fixed row structure is built here rather than in .run() so the
            # tables do not appear empty and then visibly restructure on every
            # run cycle. .run() only fills the computed cells (setRow).
            if (is.null(self$options$var) || is.null(self$data))
                return()

            self$results$missingVals$addRow(rowKey="total_obs", values=list(
                metric=.("Total Observations")))
            self$results$missingVals$addRow(rowKey="missing_vals", values=list(
                metric=.("Missing Values")))
            self$results$missingVals$addRow(rowKey="complete_cases", values=list(
                metric=.("Complete Cases")))
            self$results$missingVals$addRow(rowKey="unique_vals", values=list(
                metric=.("Unique Values")))

            variable <- self$data[[self$options$var]]

            # The outlier method-summary table carries a fixed row set - one row
            # per detection method - so it is built here too. Built with addRow()
            # in .run() against a `rows: 0` declaration it had no row names at
            # Table$fromProtoBuf time, so it could never inherit the previous
            # run's cells: it collapsed to empty and re-expanded on every run
            # cycle. .run() now only sets the computed counts.
            if (isTRUE(self$options$showOutliers) && is.numeric(variable)) {
                self$results$outlierMethodSummary$addRow(rowKey="zscore", values=list(
                    method=.("Z-score"),
                    threshold="|z| > 3",
                    note=.("Assumes normal distribution; inflated by outliers themselves")))
                self$results$outlierMethodSummary$addRow(rowKey="iqr", values=list(
                    method=.fmt(.("IQR (1.5{times}IQR)"), times = "\u{D7}"),
                    threshold=.fmt(.("< Q1-1.5{times}IQR or > Q3+1.5{times}IQR"), times = "\u{D7}"),
                    note=.("Robust to non-normality")))
                self$results$outlierMethodSummary$addRow(rowKey="mad", values=list(
                    method=.("Modified Z-score (MAD)"),
                    threshold="|modified-z| > 3.5",
                    note=.("Most robust to outliers and skewness")))
            }

            if (!self$options$showDistribution)
                return()

            # The distribution table carries one of two fixed row sets, chosen by
            # the variable's type. The type is already known at init time (the
            # header-only dataset carries column classes), so the choice is made
            # here with exactly the predicates .populateDistributionAnalysis uses.
            if (is.numeric(variable)) {
                metrics <- c(
                    mean=.("Mean"),
                    median=.("Median"),
                    std_dev=.("Standard Deviation"),
                    mad=.("MAD (Median Abs. Deviation)"),
                    coeff_var=.("Coefficient of Variation (%)"),
                    skewness=.("Skewness"),
                    range=.("Range"),
                    iqr=.("Interquartile Range (IQR)"))
            } else if (is.factor(variable) || is.character(variable)) {
                metrics <- c(
                    num_categories=.("Number of Categories"),
                    modal_category=.("Modal Category (frequency)"),
                    balance_index=.("Category Balance Index (Entropy)"))
            } else {
                metrics <- character(0)
            }

            for (key in names(metrics))
                self$results$distribution$addRow(rowKey=key, values=list(
                    metric=metrics[[key]]))
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

                # Skewness via consistent population moments (g1)
                skewness <- private$.computeSkewness(clean_var)

                # FIXED: CV calculation with stability check
                cv_min_mean <- self$options$cvMinMean
                cv_valid <- abs(mean_val) >= cv_min_mean
                cv <- ifelse(cv_valid && mean_val != 0, abs(sd_val / mean_val) * 100, NA)

                self$results$distribution$setRow(rowKey="mean", values=list(
                    value=round(mean_val, 4),
                    interpretation=ifelse(!is.na(cv) && cv < 10, .("Stable central value"),
                                        ifelse(!is.na(cv), .("Variable central tendency"), .("Central tendency")))
                ))

                self$results$distribution$setRow(rowKey="median", values=list(
                    value=round(median_val, 4),
                    # sd_val is 0 for a constant variable; the ratio below would
                    # be NaN and leave the cell blank.
                    interpretation=if (sd_val == 0) .("Equal to mean (constant value)")
                                   else if (abs(mean_val - median_val) / sd_val < 0.2) .("Close to mean (symmetric)")
                                   else .("Different from mean (skewed)")
                ))

                self$results$distribution$setRow(rowKey="std_dev", values=list(
                    value=round(sd_val, 4),
                    interpretation=.fmt(.("Absolute variability (see also MAD: {mad})"), mad = sprintf("%.3f", mad_val))
                ))

                # Add MAD as a robust alternative to SD
                self$results$distribution$setRow(rowKey="mad", values=list(
                    value=round(mad_val, 4),
                    interpretation=.("Robust spread measure (resistant to outliers)")
                ))

                # CV with improved context
                if (!is.na(cv)) {
                    cv_interpretation <- ifelse(sd_val == 0, .("No variability (constant value)"),
                                        ifelse(cv < 10, .("Low relative variability"),
                                        ifelse(cv < 20, .("Moderate relative variability"),
                                              ifelse(cv < 50, .("High relative variability"),
                                                    .("Very high relative variability")))))
                    self$results$distribution$setRow(rowKey="coeff_var", values=list(
                        value=round(cv, 2),
                        interpretation=.fmt(.("{level} - appropriate for ratio scale data"), level = cv_interpretation)
                    ))
                } else {
                    self$results$distribution$setRow(rowKey="coeff_var", values=list(
                        value=NA,
                        interpretation=.fmt(.("Suppressed (|mean| < {min}); use MAD or IQR for spread"), min = sprintf("%.3f", cv_min_mean))
                    ))
                }

                self$results$distribution$setRow(rowKey="skewness", values=list(
                    value=round(skewness, 3),
                    interpretation=private$.interpretSkewness(skewness)
                ))

                # Enhanced range analysis with outlier context
                min_val <- min(clean_var)
                max_val <- max(clean_var)
                range_val <- max_val - min_val

                self$results$distribution$setRow(rowKey="range", values=list(
                    value=round(range_val, 4),
                    interpretation=.fmt(.("From {min} to {max}"), min = sprintf("%.3f", min_val), max = sprintf("%.3f", max_val))
                ))

                # Add quartile information (robust percentiles)
                q1 <- quantile(clean_var, 0.25)
                q3 <- quantile(clean_var, 0.75)
                iqr <- q3 - q1

                self$results$distribution$setRow(rowKey="iqr", values=list(
                    value=round(iqr, 4),
                    interpretation=.fmt(.("Q1: {q1}, Q3: {q3} - robust spread metric"), q1 = sprintf("%.3f", q1), q3 = sprintf("%.3f", q3))
                ))

            } else if (is_categorical && n_complete >= 1) {
                # Distribution analysis for categorical variables. Drop zero-count
                # entries so unused factor levels do not inflate the category
                # count or produce NaN entropy (0 * log2(0)).
                clean_var <- variable[!is.na(variable)]
                freq_table <- table(clean_var)
                freq_table <- freq_table[freq_table > 0]
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

                self$results$distribution$setRow(rowKey="num_categories", values=list(
                    value=n_categories,
                    interpretation=ifelse(n_categories <= 5, .("Manageable number of categories"),
                                        ifelse(n_categories <= 10, .("Moderate number of categories"),
                                              .("Many categories - consider grouping")))
                ))

                self$results$distribution$setRow(rowKey="modal_category", values=list(
                    value=as.numeric(modal_freq),
                    interpretation=.fmt(.("Most frequent category: {cat} ({freq} of {total}, {pct}%)"),
                                        cat = modal_category, freq = modal_freq, total = n_complete,
                                        pct = sprintf("%.1f", modal_pct))
                ))

                self$results$distribution$setRow(rowKey="balance_index", values=list(
                    value=round(balance_index, 3),
                    interpretation=.fmt(.("{entropy} of {max} max entropy; {balance}"),
                                        entropy = sprintf("%.2f", entropy), max = sprintf("%.2f", max_entropy),
                                        balance = ifelse(balance_index > 0.8, .("well balanced"),
                                                  ifelse(balance_index > 0.6, .("moderately balanced"), .("imbalanced"))))
                ))

                if (rare_categories > 0) {
                    self$results$distribution$addRow(rowKey="rare_categories", values=list(
                        metric=.("Rare Categories"),
                        value=rare_categories,
                        interpretation=.fmt(.("{n} categories with <{pct}% frequency - may violate chi-squared assumptions (expected cell count {ge}5)"),
                                            n = rare_categories, pct = sprintf("%.1f", rare_threshold_pct), ge = "\u{2265}")
                    ))
                }

                # Dominant category concern
                if (modal_pct > 80) {
                    self$results$distribution$addRow(rowKey="dominance_warning", values=list(
                        metric=.("Dominance Warning (% in modal category)"),
                        value=as.numeric(modal_pct),
                        interpretation=.fmt(.("One category ('{cat}') holds {pct}% of complete cases; check whether that reflects the population or a data-entry default"),
                                            cat = modal_category, pct = sprintf("%.1f", modal_pct))
                    ))
                }

            } else {
                # .init() creates the row set from the variable's TYPE alone, but
                # the fill conditions above are narrower. Without this branch a
                # numeric variable with a single complete case - or a date,
                # date-time or logical column, which is neither numeric nor
                # factor/character - rendered a visible table whose Value and
                # Interpretation cells were all blank, with nothing to say why.
                reason <- if (is_numeric) {
                    .fmt(.("Distribution statistics need at least 2 complete numeric observations; this variable has {n}."),
                         n = n_complete)
                } else if (is_categorical) {
                    .("Distribution statistics need at least 1 complete observation; every value of this variable is missing.")
                } else {
                    paste(.("Distribution statistics are computed for numeric and for factor/character variables only."),
                          .("This variable is neither (for example a date, date-time or logical column); convert it to a numeric or a nominal variable to describe it here."))
                }
                self$results$distribution$setNote("notComputable", reason)
                for (key in self$results$distribution$rowKeys)
                    self$results$distribution$setRow(rowKey=key, values=list(
                        value=NA, interpretation=.("Not computable")))
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
                private$.outlier_transform <- outlier_analysis$transform_applied
                # Detection actually ran; only now may a count of 0 be reported
                # as "nothing was flagged" rather than "nothing was looked at".
                private$.outliers_assessed <- TRUE
                private$.outliers_informative_only <- isTRUE(outlier_analysis$is_informative_only)
                private$.outlier_n_methods <- outlier_analysis$n_methods

                # Show informative-only warning if small sample
                if (!is.null(outlier_analysis$is_informative_only) && outlier_analysis$is_informative_only) {
                    # Update table title to show informative-only status
                    self$results$outliers$setTitle(paste0(.("Outlier Detection"), " - ", outlier_analysis$consensus_note))
                    self$results$outlierMethodSummary$setTitle(.("Method Summary (INFORMATIVE ONLY - n<10)"))
                }

                # Populate method summary table (always shown when outlier
                # analysis runs). The rows were created in .init(); only the
                # computed counts are set here, so the table keeps its shape
                # across run cycles.
                self$results$outlierMethodSummary$setVisible(TRUE)

                self$results$outlierMethodSummary$setRow(rowKey="zscore", values=list(
                    outliers_detected=length(outlier_analysis$all_methods$zscore$indices)))

                self$results$outlierMethodSummary$setRow(rowKey="iqr", values=list(
                    outliers_detected=length(outlier_analysis$all_methods$iqr$indices)))

                if (!is.null(outlier_analysis$all_methods$mad)) {
                    self$results$outlierMethodSummary$setRow(rowKey="mad", values=list(
                        outliers_detected=length(outlier_analysis$all_methods$mad$indices)))
                } else {
                    self$results$outlierMethodSummary$setRow(rowKey="mad", values=list(
                        outliers_detected=NA,
                        note=.("Not computed: needs more than 3 complete values and a non-zero MAD; detection here rests on the other two methods")))
                }

                # Transformation status is a table note rather than a fourth row,
                # so the row set stays fixed. A note is always written (never
                # left over from a previous run's option setting).
                transform_status <- if (!is.null(outlier_analysis$transform_skipped)) {
                    outlier_analysis$transform_skipped
                } else if (outlier_analysis$transform_applied != "none") {
                    .fmt(.("Counts were computed after a {trans} transformation. Flagged values are reported on the original scale, while thresholds and scores are on the {trans} scale, so do not compare a reported bound directly against a reported value."),
                         trans = outlier_analysis$transform_applied)
                } else {
                    .("Counts were computed on the raw (untransformed) values.")
                }
                self$results$outlierMethodSummary$setNote("transform", transform_status)

                if (outliers_found > 0) {
                    # Show outliers table, hide no outliers message
                    self$results$outliers$setVisible(TRUE)
                    self$results$noOutliers$setVisible(FALSE)

                    # Get original row numbers. These index the analysis
                    # dataset, i.e. the rows AFTER any jamovi row filter, so say
                    # so - the surrounding text asks the user to go and check
                    # each flagged row in the spreadsheet.
                    complete_indices <- which(!is.na(variable))
                    self$results$outliers$setNote("rowRef", .("Row numbers refer to the rows included in this analysis. If a row filter is active they will not match the spreadsheet row numbers."))

                    for (i in seq_along(outlier_analysis$outlier_indices)) {
                        outlier_idx <- outlier_analysis$outlier_indices[i]
                        original_row <- complete_indices[outlier_idx]

                        z_score_transformed <- outlier_analysis$transformed_z_scores[outlier_idx]
                        confidence_level <- outlier_analysis$detection_count[i]

                        # Rank magnitude on the most robust statistic actually
                        # available for this point. The ordinary Z-score is
                        # bounded above by (n-1)/sqrt(n) - only 1.79 at n = 5 -
                        # and is itself inflated by the outlier it is measuring,
                        # so ranking on it understated (and at small n outright
                        # contradicted) every point flagged by the other two
                        # methods. The MAD-based modified Z-score is not bounded
                        # that way and is on the same nominal sigma scale.
                        if (!is.null(outlier_analysis$modified_z_scores)) {
                            severity_score <- outlier_analysis$modified_z_scores[outlier_idx]
                            severity_basis <- .("modified Z")
                        } else {
                            severity_score <- z_score_transformed
                            severity_basis <- "Z"
                        }

                        # Severity text, with the scale named when a transform
                        # was applied (two complete sentences, not a spliced
                        # ", log scale" fragment).
                        severity_text <- if (outlier_analysis$transform_applied != "none") {
                            .fmt(.("{severity} (flagged by {k} of {m} methods; magnitude by {basis} on the {trans} scale)"),
                                 severity = private$.outlierSeverity(severity_score), k = confidence_level,
                                 m = outlier_analysis$n_methods, basis = severity_basis,
                                 trans = outlier_analysis$transform_applied)
                        } else {
                            .fmt(.("{severity} (flagged by {k} of {m} methods; magnitude by {basis})"),
                                 severity = private$.outlierSeverity(severity_score), k = confidence_level,
                                 m = outlier_analysis$n_methods, basis = severity_basis)
                        }

                        # Per-method flags.
                        #
                        # These were inverted: ifelse(flagged, "", " - ") printed an
                        # EMPTY cell for a method that DID detect the point and a
                        # dash for one that did not, so a row whose severity read
                        # "3/3 methods" showed three blank method columns. (The
                        # empty string looks like a tick character that was lost in
                        # a non-ASCII sweep.) Use plain words - unambiguous, and no
                        # encoding to lose.
                        method_flags <- outlier_analysis$detection_matrix[i, ]
                        flag_text <- function(detected) if (isTRUE(unname(detected))) .("Yes") else "-"
                        zscore_flag <- flag_text(method_flags["zscore"])
                        iqr_flag <- flag_text(method_flags["iqr"])
                        mad_flag <- if (is.null(outlier_analysis$all_methods$mad)) {
                            .("N/A")
                        } else {
                            flag_text(method_flags["mad"])
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
                    # Neither the threshold nor the method count may be
                    # hard-coded: the threshold is 1 below n = 10, and only 2
                    # methods run whenever the MAD is unavailable. The old text
                    # read ">=2 of the 3 methods" directly above a Method Summary
                    # whose MAD row said "Not computed".
                    min_methods <- if (isTRUE(outlier_analysis$is_informative_only)) 1L else 2L
                    flagged_sentence <- if (outlier_analysis$transform_applied != "none") {
                        .fmt(.("No value was flagged by {k} or more of the {m} methods that ran (after {trans} transformation)."),
                             k = min_methods, m = outlier_analysis$n_methods, trans = outlier_analysis$transform_applied)
                    } else {
                        .fmt(.("No value was flagged by {k} or more of the {m} methods that ran."),
                             k = min_methods, m = outlier_analysis$n_methods)
                    }
                    no_outliers_message <- paste0(
                        "<div style='padding: 12px 15px; background-color: rgba(40, 167, 69, 0.14); border-left: 4px solid #28a745; color: inherit; border-radius: 4px;'>",
                        "<p style='font-weight: bold; margin-top: 0;'>", .("No outliers detected"), "</p>",
                        "<p style='margin-bottom: 0;'>", flagged_sentence, " ",
                        .("See the Method Summary table for what each method flagged on its own."), " ",
                        .("This does not establish that the variable is free of erroneous values: these methods detect isolated extreme values, not miscoded values that fall inside the observed range."),
                        "</p></div>"
                    )
                    self$results$noOutliers$setContent(no_outliers_message)
                }
            } else if (is_numeric && n_complete < 3) {
                # Insufficient data for outlier detection
                self$results$outliers$setVisible(FALSE)
                self$results$outlierMethodSummary$setVisible(FALSE)
                self$results$noOutliers$setVisible(TRUE)
                no_outliers_message <- paste0(
                    "<div style='padding: 12px 15px; background-color: rgba(255, 193, 7, 0.16); border-left: 4px solid #ffc107; color: inherit; border-radius: 4px;'>",
                    "<p style='font-weight: bold; margin-top: 0;'>", .("Insufficient data for outlier detection"), "</p>",
                    "<p style='margin-bottom: 0;'>",
                    .fmt(.("At least 3 complete observations are required; this variable has n={n}."), n = n_complete), " ",
                    .("No outlier screening was performed, so the Outliers component of the quality grade below carries no penalty and the grade says nothing about extreme values here."),
                    "</p></div>")
                self$results$noOutliers$setContent(no_outliers_message)
            } else if (!is_numeric) {
                # Non-numeric variables - explain why outlier detection isn't applicable
                self$results$outliers$setVisible(FALSE)
                self$results$outlierMethodSummary$setVisible(FALSE)
                self$results$noOutliers$setVisible(TRUE)
                no_outliers_message <- paste0(
                    "<div style='padding: 12px 15px; background-color: rgba(23, 162, 184, 0.14); border-left: 4px solid #17a2b8; color: inherit; border-radius: 4px;'>",
                    "<p style='font-weight: bold; margin-top: 0;'>", .("Outlier detection not applicable"), "</p>",
                    "<p style='margin-bottom: 0;'>",
                    .("Outlier analysis is defined for numeric variables only; this variable is not numeric."), " ",
                    .("The Outliers component of the quality grade below carries no penalty as a result."),
                    "</p></div>")
                self$results$noOutliers$setContent(no_outliers_message)
            }

            return(outliers_found)
        },

        .run = function() {
            private$.outlier_transform <- "none"
            private$.outliers_assessed <- FALSE
            private$.outliers_informative_only <- FALSE
            private$.outlier_n_methods <- 3

            # Assigned only inside the numeric distribution block below. It is
            # initialised here so the limitations text can test !is.null(skewness)
            # instead of exists("skewness"): exists() searches the enclosing
            # environment chain, which for an R6 method reaches this package's
            # imports environment, where NAMESPACE's
            # importFrom(moments, kurtosis, skewness) makes the name resolve to a
            # FUNCTION. The guard was therefore always TRUE, and one change to
            # the surrounding condition would have handed a closure to abs().
            skewness <- NULL

            # TODO (forward-looking, perf): add checkpoints around
            # `.populateOutlierAnalysis`, `.analyzeMissingPatterns`,
            # `.analyzeCategoricalQuality`, and `.clinicalContextValidation`.
            # TODO (cleanup): file is 2.2k LOC - split into helper files
            # (`.checkOutliers`, `.checkDistribution`, `.checkPlausibility`,
            # `.renderHtml`) to keep each unit under ~500 LOC.

            # Control visibility based on variable selection
            variable_selected <- !is.null(self$options$var)

            # Set visibility for all items
            self$results$todo$setVisible(!variable_selected)
            # `notices` (Important Information) is shown only once quality-threshold
            # alerts are actually rendered near the end of a successful .run.
            self$results$notices$setVisible(FALSE)
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
                todo_content <- paste0(
                    "<h3>", .("ClinicoPath Data Quality Assessment"), "</h3>",
                    "<p>", .("<strong>Purpose:</strong> Comprehensive evaluation of data completeness, accuracy, and patterns for clinical research."), "</p>",
                    "<p>", .fmt(.("<strong>IMPORTANT:</strong> Outlier detection uses a <strong>consensus approach</strong> - points are only flagged if detected by {ge}2 of 3 methods (Z-score, IQR, Modified Z-score). Points flagged by only 1 method are <strong>not shown</strong>, even if they exceed |z|>3."), ge = "\u{2265}"), " ",
                    .("Two exceptions: below n = 10 single-method flags ARE shown, labelled informative-only and excluded from the quality grade; and the Modified Z-score needs more than 3 complete values and a non-zero MAD, so only the other two methods run when it is unavailable."), "</p>",
                    "<h4>", .("Required Input:"), "</h4>",
                    "<ul><li>", .("<strong>Variable to Check:</strong> Select any variable for quality assessment"), "</li></ul>",
                    "<h4>", .("Analysis Options:"), "</h4>",
                    "<ul>",
                    "<li>", .fmt(.("<strong>Outlier Analysis:</strong> Consensus-based detection ({ge}2 of 3 methods: Z-score |z|>3, IQR 1.5{times}rule, Modified Z-score |z|>3.5)"), ge = "\u{2265}", times = "\u{D7}"), "</li>",
                    "<li>", .("<strong>Distribution Analysis:</strong> Descriptive statistics, robust spread (MAD, IQR), coefficient of variation and skewness"), "</li>",
                    "<li>", .("<strong>Duplicate Analysis:</strong> Identify repeated values and patterns"), "</li>",
                    "<li>", .("<strong>Pattern Analysis:</strong> Missing data mechanisms and systematic issues"), "</li>",
                    "</ul>",
                    "<h4>", .("Assessment Dimensions:"), "</h4>",
                    "<ul>",
                    "<li>", .("<strong>Completeness:</strong> Missing data evaluation and impact assessment"), "</li>",
                    "<li>", .("<strong>Accuracy:</strong> Outlier detection and range validation"), "</li>",
                    "<li>", .("<strong>Consistency:</strong> Pattern recognition and systematic issues"), "</li>",
                    "<li>", .("<strong>Clinical Validity:</strong> Context-specific validation (age, lab values, etc.)"), "</li>",
                    "</ul>",
                    "<h4>", .("Quality Grading:"), "</h4>",
                    "<ul>",
                    "<li>", .("<strong>Grade A:</strong> Excellent quality - ready for analysis"), "</li>",
                    "<li>", .("<strong>Grade B:</strong> Good quality - minor issues documented"), "</li>",
                    "<li>", .("<strong>Grade C:</strong> Concerning quality - cleaning recommended"), "</li>",
                    "<li>", .("<strong>Grade D:</strong> Poor quality - major intervention required"), "</li>",
                    "</ul>",
                    "<h4>", .("Clinical Applications:"), "</h4>",
                    "<ul>",
                    "<li>", .("<strong>Clinical Trials:</strong> Regulatory compliance and data monitoring"), "</li>",
                    "<li>", .("<strong>Observational Studies:</strong> Data integrity assessment"), "</li>",
                    "<li>", .("<strong>Quality Improvement:</strong> Systematic quality monitoring"), "</li>",
                    "<li>", .("<strong>Publication Preparation:</strong> Data quality documentation"), "</li>",
                    "</ul>"
                )
                self$results$todo$setContent(todo_content)
                return()
            }
            
            # Get variable data with enhanced validation
            variable <- self$data[[self$options$var]]
            var_name <- self$options$var

            # Comprehensive data validation. An empty dataset - no rows, or a row
            # filter that excludes every case - arrives here as a zero-length
            # variable and is reported by .validateData, so this is the SINGLE
            # error exit. .run() used to carry its own nrow(self$data) == 0 test
            # a few lines above, whose early return made this branch unreachable.
            validation_results <- private$.validateData(variable, var_name)

            # Fatal validation failure (the variable has no observations). Raised
            # as an analysis-level error so jamovi greys the results in place with
            # its own error presentation, instead of hiding eight elements and
            # painting a red box into the welcome panel.
            if (!validation_results$is_valid)
                jmvcore::reject(paste(validation_results$error_messages, collapse = " "), code = NULL)

            # Infinite values are treated as missing from here on. .validateData
            # has already counted them; left in place they turn mean(), sd() and
            # the skewness moments into NaN, and `if (NaN <= 0)` aborted the run
            # with a raw R error after the warning had been raised.
            n_infinite <- if (is.numeric(variable)) sum(is.infinite(variable)) else 0L
            if (n_infinite > 0)
                variable[is.infinite(variable)] <- NA
            
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
            self$results$missingVals$setRow(rowKey="total_obs", values=list(
                value=as.character(n_total),
                interpretation=ifelse(n_total >= 100, .("Adequate sample size"),
                                    ifelse(n_total >= 30, .("Moderate sample size"), .("Small sample size")))
            ))

            self$results$missingVals$setRow(rowKey="missing_vals", values=list(
                value=sprintf("%d (%.1f%%)", n_missing, missing_pct),
                interpretation=private$.interpretMissing(missing_pct)
            ))

            self$results$missingVals$setRow(rowKey="complete_cases", values=list(
                value=sprintf("%d (%.1f%%)", n_complete, 100-missing_pct),
                interpretation=ifelse(n_complete >= 0.9 * n_total, .("Excellent completeness"),
                                    ifelse(n_complete >= 0.8 * n_total, .("Good completeness"),
                                          ifelse(n_complete >= 0.7 * n_total, .("Acceptable completeness"), .("Poor completeness"))))
            ))

            self$results$missingVals$setRow(rowKey="unique_vals", values=list(
                value=sprintf("%d (%.1f%%)", n_unique, unique_pct),
                interpretation=ifelse(unique_pct > 95, .("Very high variability"),
                                    ifelse(unique_pct > 50, .("High variability"),
                                          ifelse(unique_pct > 10, .("Moderate variability"), .("Low variability"))))
            ))

            if (n_infinite > 0)
                self$results$missingVals$setNote("infinite", .fmt(
                    .("{n} infinite values (Inf or -Inf) are counted as missing."), n = n_infinite))

            # Advanced outlier detection for numeric variables
            # Use the refactored .populateOutlierAnalysis method
            outliers_found <- private$.populateOutlierAnalysis(variable, is_numeric, n_complete)

            # Two different questions, kept apart deliberately:
            #   outliers_assessed  - did detection run at all? (0 flags means
            #                        "nothing flagged" only when it did)
            #   outliers_scored    - is the result robust enough to move the
            #                        grade? Below n = 10 detection falls back to
            #                        single-method flags that the detector itself
            #                        labels "not statistically robust", so those
            #                        flags are reported but not scored and do not
            #                        raise outlier-rate warnings.
            outliers_assessed <- isTRUE(private$.outliers_assessed)
            outliers_informative_only <- isTRUE(private$.outliers_informative_only)
            outliers_scored <- outliers_assessed && !outliers_informative_only

            # Enhanced distribution analysis for numeric and categorical variables
            # Use the refactored .populateDistributionAnalysis method
            if (self$options$showDistribution) {
                private$.populateDistributionAnalysis(variable, is_numeric, is_categorical, n_complete)
            }

            # Enhanced duplicate analysis with categorical support
            if (self$options$showDuplicates && n_complete > 0) {
                clean_var <- variable[!is.na(variable)]
                freq_table <- table(clean_var)
                
                # Enhanced duplicate detection
                if (is_categorical) {
                    # For categorical data this lists every category, including
                    # those occurring once - a frequency table, not duplicates -
                    # so the heading is corrected to match what is shown. The
                    # numeric branch below does filter to freq > 1.
                    self$results$duplicates$setTitle(.("Value Frequencies (all categories)"))
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
                    if (length(freq_table_sorted) > max_display)
                        self$results$duplicates$setNote("truncated", .fmt(
                            .("Showing the {max} most frequent of {total} categories."),
                            max = max_display, total = length(freq_table_sorted)))
                } else {
                    # For numeric data, show only duplicates
                    self$results$duplicates$setTitle(.("Duplicate Values"))
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
                        if (length(duplicates) > max_display)
                            self$results$duplicates$setNote("truncated", .fmt(
                                .("Showing the {max} most frequent of {total} duplicated values."),
                                max = max_display, total = length(duplicates)))
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
                        pattern=.fmt(.("Missing Data: {kind}"), kind = stringr::str_to_title(gsub("_", " ", pattern_name))),
                        description=missing_patterns[[pattern_name]],
                        recommendation=ifelse(missing_pct > 20, .("Investigate missing data mechanisms"),
                                            .("Document missing data pattern"))
                    ))
                    pattern_count <- pattern_count + 1
                }
                
                # Categorical data quality patterns
                if (is_categorical) {
                    categorical_issues <- private$.analyzeCategoricalQuality(variable)
                    if (!is.null(categorical_issues)) {
                        for (issue_name in names(categorical_issues)) {
                            self$results$patterns$addRow(rowKey=pattern_count, values=list(
                                pattern=.fmt(.("Categorical: {kind}"), kind = stringr::str_to_title(gsub("_", " ", issue_name))),
                                description=categorical_issues[[issue_name]],
                                recommendation=ifelse(grepl("imbalance", issue_name),
                                                    .("Consider sampling strategy or analysis method"),
                                                    .("Review category definitions and data entry"))
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
                            pattern=.fmt(.("Clinical Validation: {kind}"), kind = stringr::str_to_title(gsub("_", " ", issue_name))),
                            description=clinical_issues[[issue_name]],
                            recommendation=.("Verify clinical plausibility and measurement units")
                        ))
                        pattern_count <- pattern_count + 1
                    }
                    # Attach the caveat to the findings themselves: a row reading
                    # "Clinical Validation: Implausible Age" is a heuristic verdict,
                    # and the panel that explains that is off by default.
                    self$results$patterns$setNote(
                        "clinicalHeuristic",
                        paste(
                            .("<b>Clinical Validation rows are heuristic screening flags, not clinical judgements.</b>"),
                            .("Plausibility bounds come from general-population rules of thumb rather than validated reference ranges, and may not suit paediatric, ICU, oncology or athlete populations, or differing measurement methods and demographics."),
                            .("Which checks run is decided by pattern-matching the variable NAME (for example 'age', 'glucose', 'systolic'), so non-standard naming can silently skip a check or apply the wrong one."),
                            .fmt(.("Units are inferred from the data range (centimetres against metres, mg/dL against {mu}mol/L); a misread unit will flag correct values as implausible."), mu = "\u{B5}"),
                            .("Confirm every flag against your own study protocol before excluding or correcting a value."),
                            sep = " "))
                }
                
                # Data validation warnings integration
                if (length(validation_results$warnings) > 0) {
                    for (warning_msg in validation_results$warnings) {
                        self$results$patterns$addRow(rowKey=pattern_count, values=list(
                            pattern=.("Data Validation Warning"),
                            description=warning_msg,
                            recommendation=.("Review data collection procedures")
                        ))
                        pattern_count <- pattern_count + 1
                    }
                }
                
                # Low uniqueness pattern (enhanced)
                if (n_complete > 0 && (n_unique / n_complete < 0.1)) {
                    uniqueness_interpretation <- ifelse(n_unique == 1, .("Constant value detected"),
                                                      ifelse(n_unique / n_complete < 0.05, .("Very low uniqueness"),
                                                            .("Low uniqueness")))
                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern=.("Data Variability"),
                        description=.fmt(.("{label}: {pct}% unique values ({nuniq}/{ncomp})"),
                                         label = uniqueness_interpretation, pct = sprintf("%.1f", unique_pct),
                                         nuniq = n_unique, ncomp = n_complete),
                        recommendation=ifelse(n_unique == 1, .("Investigate constant value cause"), 
                                            .("Verify if low variability reflects true data structure"))
                    ))
                    pattern_count <- pattern_count + 1
                }
                
                # High outlier rate pattern (enhanced)
                if (outliers_scored && outliers_found > 0.05 * n_complete) {
                    outlier_rate_pct <- round(100 * outliers_found / n_complete, 1)
                    severity_desc <- ifelse(outlier_rate_pct > 15, .("Very high"),
                                          ifelse(outlier_rate_pct > 10, .("High"), .("Elevated")))

                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern=.("Outlier Pattern"),
                        description=.fmt(.("{level} outlier rate: {n} outliers ({pct}% of data)"),
                                         level = severity_desc, n = outliers_found, pct = sprintf("%.1f", outlier_rate_pct)),
                        recommendation=.("Investigate measurement procedures and consider robust analysis methods")
                    ))
                    pattern_count <- pattern_count + 1
                }

                # Additional patterns for numeric variables
                if (is_numeric && n_complete >= 2) {
                    clean_var <- variable[!is.na(variable)]

                    # Distribution shape pattern
                    if (length(clean_var) > 3) {
                        skewness <- private$.computeSkewness(clean_var)

                        if (abs(skewness) > 1) {
                            skew_direction <- ifelse(skewness > 0, .("right-skewed"), .("left-skewed"))
                            self$results$patterns$addRow(rowKey=pattern_count, values=list(
                                pattern=.("Distribution Shape"),
                                description=.fmt(.("Highly {direction} distribution (skewness: {skew})"), direction = skew_direction, skew = sprintf("%.2f", skewness)),
                                recommendation=.("Consider data transformation or non-parametric methods")
                            ))
                            pattern_count <- pattern_count + 1
                        }
                    }

                    # Range and precision patterns
                    range_val <- max(clean_var) - min(clean_var)
                    if (all(clean_var == round(clean_var))) {
                        self$results$patterns$addRow(rowKey=pattern_count, values=list(
                            pattern=.("Data Precision"),
                            description=.("All values are integers (whole numbers)"),
                            recommendation=.("Verify if decimal precision is needed for analysis")
                        ))
                        pattern_count <- pattern_count + 1
                    }

                    # Concentration patterns (clustering)
                    if (length(unique(clean_var)) < n_complete * 0.5 && length(unique(clean_var)) > 2) {
                        concentration_pct <- round(100 * length(unique(clean_var)) / n_complete, 1)
                        self$results$patterns$addRow(rowKey=pattern_count, values=list(
                            pattern=.("Value Concentration"),
                            description=.fmt(.("Moderate value clustering: {pct}% unique values"), pct = sprintf("%.1f", concentration_pct)),
                            recommendation=.("Check for rounding, grouping, or measurement intervals")
                        ))
                        pattern_count <- pattern_count + 1
                    }
                }

                # If no patterns were found, add a general assessment
                if (pattern_count == 1) {
                    self$results$patterns$addRow(rowKey=pattern_count, values=list(
                        pattern=.("Overall Assessment"),
                        description=.("No significant data quality issues detected"),
                        recommendation=.("Data appears suitable for standard statistical analysis")
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
                quality_issues <- c(quality_issues, .("severe missing data (>50%)"))
            } else if (missing_pct > 30) {
                missing_penalty <- 25
                quality_issues <- c(quality_issues, .("extensive missing data (30-50%)"))
            } else if (missing_pct > 15) {
                missing_penalty <- 15
                quality_issues <- c(quality_issues, .("substantial missing data (15-30%)"))
            } else if (missing_pct > 5) {
                missing_penalty <- 5
            }
            component_scores$missing <- list(penalty = missing_penalty, max_penalty = 40,
                                             description = .fmt(.("Missing {pct}%"), pct = sprintf("%.1f", missing_pct)))
            quality_score <- quality_score - missing_penalty

            # Component 2: Outlier assessment (max penalty: 30 points)
            #
            # .populateOutlierAnalysis returns 0 both when nothing was flagged and
            # when nothing was looked at (checkbox unticked, non-numeric variable,
            # or n < 3). Scoring cannot tell those apart from the count alone, so
            # the not-assessed case is now carried explicitly: no penalty is
            # charged for a check that never ran, and the breakdown says so rather
            # than reporting a fabricated "Outlier rate 0.0%". Unticking a
            # display-only checkbox used to raise the headline grade by up to 30
            # points and assert a fact the analysis had not checked.
            outlier_rate <- ifelse(outliers_assessed && n_complete > 0, outliers_found / n_complete, 0)
            outlier_penalty <- 0
            if (outliers_scored) {
                if (outlier_rate > 0.15) {
                    outlier_penalty <- 30
                    quality_issues <- c(quality_issues, .("very high outlier rate (>15%)"))
                } else if (outlier_rate > 0.10) {
                    outlier_penalty <- 20
                    quality_issues <- c(quality_issues, .("high outlier rate (10-15%)"))
                } else if (outlier_rate > 0.05) {
                    outlier_penalty <- 10
                    quality_issues <- c(quality_issues, .("elevated outlier rate (5-10%)"))
                }
            }
            outlier_skip_reason <- if (!isTRUE(self$options$showOutliers)) {
                .("the Outlier analysis option is switched off")
            } else if (!is_numeric) {
                .("this is not a numeric variable")
            } else if (!outliers_assessed) {
                .fmt(.("outlier detection needs at least 3 complete values and this variable has {n}"), n = n_complete)
            } else if (outliers_informative_only) {
                .fmt(.("below n = 10 (here n = {n}) only single-method flags are available and they are not statistically robust, so they are reported but not scored"), n = n_complete)
            } else {
                ""   # unreachable: this branch means detection ran and was scored
            }
            component_scores$outliers <- list(
                penalty = outlier_penalty, max_penalty = 30,
                description = if (outliers_scored) .fmt(.("Outlier rate {pct}%"), pct = sprintf("%.1f", outlier_rate * 100))
                              else .fmt(.("NOT ASSESSED - {reason}"), reason = outlier_skip_reason))
            quality_score <- quality_score - outlier_penalty

            # Component 3: Variability assessment (max penalty: 25 points)
            #
            # Scored on the NUMBER of distinct values, not on the ratio
            # n_unique / n_complete. The ratio makes the penalty grow with sample
            # size for any variable with a fixed set of levels: on the bundled
            # histopathology data (n = 250) Sex, Group, LVI, PNI and Death each
            # have 2 distinct values, ratio 0.008, and took the full 25-point
            # penalty - Grade C, "Quality Concerns Detected", for a complete and
            # clean binary column - while the same column at n = 125 would have
            # scored Grade B. The headline grade was being driven by n, not by
            # quality. A count of distinct values does not depend on n.
            #
            # Only a constant is penalised: one distinct value means the column
            # carries no information at all. A small number of levels is a
            # property of the measurement rather than a defect, and is already
            # described, with its percentage, in the Data Patterns table and in
            # the VARIABLE CHARACTERISTICS block. Categorical variables are not
            # assessed at all - "few levels" is what a factor IS.
            variability_penalty <- 0
            variability_assessed <- n_complete > 0 && !is_categorical
            if (variability_assessed && n_unique == 1) {
                variability_penalty <- 25
                quality_issues <- c(quality_issues, .("no variability (a single distinct value)"))
            }
            component_scores$variability <- list(
                penalty = variability_penalty, max_penalty = 25,
                description = if (is_categorical) {
                    .("NOT ASSESSED - categorical variable")
                } else if (n_complete == 0) {
                    .("NOT ASSESSED - no complete observations")
                } else if (n_unique == 1) {
                    .fmt(.("Constant: 1 distinct value across {n} complete observations"), n = n_complete)
                } else {
                    .fmt(.("{nuniq} distinct values across {n} complete observations"), nuniq = n_unique, n = n_complete)
                })
            quality_score <- quality_score - variability_penalty

            # Component 4: Clinical validity assessment (max penalty: 20 points, if enabled)
            clinical_penalty <- 0
            clinical_issues_found <- private$.clinicalContextValidation(variable, var_name)
            # Entries that only report "units could not be auto-detected" are not
            # plausibility failures and carry no penalty, so the printed
            # description has to count the SAME set the penalty counts. It used
            # the unfiltered list, so a Weight column of undetectable units
            # printed "-0 / 20 pts  (1 plausibility checks failed)" - a
            # self-contradicting row in the block that exists to explain the
            # grade. Hoisted out of the if so there is one definition for the
            # penalty, the description and the notice below.
            penalizable_clinical <- list()
            if (!is.null(clinical_issues_found) && length(clinical_issues_found) > 0) {
                penalizable_clinical <- clinical_issues_found[
                    !grepl("auto-detect|could not auto", clinical_issues_found, ignore.case = TRUE)]
                if (length(penalizable_clinical) > 0) {
                    clinical_penalty <- min(20, length(penalizable_clinical) * 5)
                    quality_issues <- c(quality_issues, .fmt(.("clinical plausibility issues ({n} checks failed)"), n = length(penalizable_clinical)))
                } else {
                    quality_issues <- c(quality_issues, .("clinical units unclear (not penalized)"))
                }
            }
            component_scores$clinical <- list(penalty = clinical_penalty, max_penalty = 20,
                                              description = .fmt(.("{n} plausibility checks failed"),
                                                                 n = length(penalizable_clinical)))
            quality_score <- quality_score - clinical_penalty

            # Component 5: Sample size assessment (max penalty: 30 points)
            sample_penalty <- 0
            if (n_total < 10) {
                sample_penalty <- 30
                quality_issues <- c(quality_issues, .("very small sample size (n<10)"))
            } else if (n_total < 30) {
                sample_penalty <- 15
                quality_issues <- c(quality_issues, .("small sample size (n<30)"))
            } else if (n_total < 50) {
                sample_penalty <- 5
            }
            component_scores$sample_size <- list(penalty = sample_penalty, max_penalty = 30,
                                                 description = .fmt(.("n={n}"), n = n_total))
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

            # Collect quality threshold notices as HTML for user-facing alerts.
            # Rendered into the always-available `notices` (Important Information)
            # Html output rather than via jmvcore::Notice$new + insert(), which is
            # a documented protobuf serialization risk (see
            # docs/NOTICE_TO_HTML_CONVERSION_GUIDE.md). Note: `todo` is hidden once
            # a variable is selected, so alerts must NOT be routed there.
            quality_notices_html <- list()
            # STRONG_WARNING and WARNING used to share a background, a foreground
            # and the title "Warning", differing only in a 4px border colour -
            # so "Severe missing data: 62.0% missing" rendered indistinguishably
            # from "Small sample size (n=25)". They now differ in tint and title.
            # Tints are translucent with `color: inherit` so they composite over
            # jamovi's light AND dark themes; the opaque #fff3cd / #d1ecf1 cards
            # were the last light-mode-only panels left in this file.
            .noticeBox <- function(level, msg) {
                cfg <- switch(level,
                    STRONG_WARNING = list(bg = "rgba(255, 152, 0, 0.20)", border = "#e65100", title = .("Important Warning")),
                    WARNING        = list(bg = "rgba(255, 193, 7, 0.14)", border = "#ffc107", title = .("Warning")),
                    INFO           = list(bg = "rgba(23, 162, 184, 0.14)", border = "#17a2b8", title = .("Note")))
                paste0(
                    "<div style='padding: 12px 15px; margin: 6px 0; background-color: ", cfg$bg,
                    "; border-left: 4px solid ", cfg$border, "; color: inherit",
                    "; border-radius: 4px;'><strong>", cfg$title, ":</strong> ", msg, "</div>")
            }

            # STRONG_WARNING: Severe missing data (>30%)
            if (missing_pct > 30) {
                quality_notices_html$severeMissing <- .noticeBox("STRONG_WARNING", sprintf(
                    .("Severe missing data: %.1f%% missing values. Results may be unreliable; investigate missing data mechanisms (MCAR/MAR/MNAR) before analysis."),
                    missing_pct))
            } else if (missing_pct > 15) {
                quality_notices_html$substantialMissing <- .noticeBox("WARNING", sprintf(
                    .("Substantial missing data: %.1f%% missing values. Consider sensitivity analysis with multiple imputation methods."),
                    missing_pct))
            }

            # STRONG_WARNING: Very high outlier rate (>15%)
            # outlier_rate is the assessed-aware value computed with Component 2;
            # recomputing it here would resurrect the not-assessed = 0% claim.
            # Gated on outliers_scored, not merely outliers_assessed: at n = 9 a
            # single non-robust IQR flag is an 11.1% "high outlier rate".
            if (!outliers_scored) {
                # no outlier-rate alert; the informative-only status is stated in
                # the table title and in the LIMITATIONS section
            } else if (outlier_rate > 0.15) {
                quality_notices_html$veryHighOutliers <- .noticeBox("STRONG_WARNING", sprintf(
                    .("Very high outlier rate: %.1f%% of data flagged as outliers. Verify measurement procedures and consider robust analysis methods."),
                    outlier_rate * 100))
            } else if (outlier_rate > 0.10) {
                quality_notices_html$highOutliers <- .noticeBox("WARNING", sprintf(
                    .("High outlier rate: %.1f%% of data flagged as outliers. Review each outlier for data entry errors and clinical plausibility."),
                    outlier_rate * 100))
            }

            # STRONG_WARNING: Very small sample (n<10)
            if (n_total < 10) {
                quality_notices_html$verySmallSample <- .noticeBox("STRONG_WARNING", sprintf(
                    .("Very small sample size (n=%d). Statistical analyses unreliable; outlier detection is informative-only. Consider collecting additional data."),
                    n_total))
            } else if (n_total < 30) {
                quality_notices_html$smallSample <- .noticeBox("WARNING", sprintf(
                    .("Small sample size (n=%d). Use appropriate methods for small samples and consider collecting additional data for robust analysis."),
                    n_total))
            }

            # STRONG_WARNING: constant variable.
            #
            # Derived from the Component 3 penalty so the notice and the grade
            # cannot disagree. The previous version recomputed the ratio as
            # ifelse(n_complete > 0, n_unique / n_complete, 0), and 0 < 0.01, so a
            # column in which EVERY value was missing raised "Extremely low
            # variability ... (0 unique out of 0)" - pointing the reader at a
            # constant-value problem that does not exist, next to the 100%-missing
            # report that does.
            if (variability_penalty == 25) {
                quality_notices_html$extremeLowVar <- .noticeBox("STRONG_WARNING", sprintf(
                    .("No variability: all %d complete observations hold the same value. Investigate the constant value or the data collection procedure."),
                    n_complete))
            }

            # WARNING: Clinical plausibility issues (if enabled and issues found)
            if (self$options$clinicalValidation && length(penalizable_clinical) > 0) {
                quality_notices_html$clinicalIssues <- .noticeBox("WARNING", sprintf(
                    .("Clinical plausibility issues: %d validation checks failed. Verify measurement units and clinical plausibility before analysis."),
                    length(penalizable_clinical)))
            }

            # INFO: Analysis complete with quality summary
            grade_desc <- ifelse(quality_score >= 90, .("Excellent"),
                         ifelse(quality_score >= 80, .("Good"),
                         ifelse(quality_score >= 70, .("Fair"), .("Poor"))))
            outlier_extra <- if (outliers_scored) ""
                else if (!outliers_assessed)
                    .(" Outlier screening did not run for this variable, so the grade excludes it and says nothing about extreme values.")
                else
                    .(" Outlier flags below n = 10 are informative-only, so the grade excludes them.")
            quality_notices_html$analysisComplete <- .noticeBox("INFO", sprintf(
                .("Quality assessment completed: %d observations analyzed. Overall quality: %s (Grade %s). Note: Scoring is heuristic-based; review component breakdown for details.%s"),
                n_total, grade_desc, quality_grade, outlier_extra))

            # Render notices in priority order: STRONG_WARNING -> WARNING -> INFO
            priority_order <- c('severeMissing', 'substantialMissing', 'veryHighOutliers', 'highOutliers',
                               'verySmallSample', 'smallSample', 'extremeLowVar', 'clinicalIssues', 'analysisComplete')
            rendered <- character()
            for (name in priority_order) {
                if (!is.null(quality_notices_html[[name]])) {
                    rendered <- c(rendered, quality_notices_html[[name]])
                }
            }
            if (length(rendered) > 0) {
                self$results$notices$setContent(paste(rendered, collapse = ""))
                self$results$notices$setVisible(TRUE)
            }

            # IMPROVED: Transparent heuristic quality summary with softened presentation
            quality_text <- sprintf("\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\n")
            quality_text <- paste0(quality_text, sprintf("   DATA QUALITY ASSESSMENT FOR '%s'\n", toupper(var_name)))
            quality_text <- paste0(quality_text, sprintf("\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\n\n"))

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

            quality_text <- paste0(quality_text, sprintf("HEURISTIC QUALITY: Grade %s - %s\n", quality_grade, score_band))
            quality_text <- paste0(quality_text, "\nIMPORTANT: This is a HEURISTIC (rule-of-thumb) assessment, NOT a validated metric.\n")
            quality_text <- paste0(quality_text, "The score uses arbitrary thresholds. Apply clinical judgment, not automated rules.\n\n")

            # Show scoring breakdown for transparency
            quality_text <- paste0(quality_text, "SCORING BREAKDOWN (shows penalty applied / maximum penalty):\n")
            quality_text <- paste0(quality_text, sprintf("\u{2022} Missing Data:      -%2d / %2d pts  (%s)\n",
                                                        component_scores$missing$penalty,
                                                        component_scores$missing$max_penalty,
                                                        component_scores$missing$description))
            quality_text <- paste0(quality_text, sprintf("\u{2022} Outliers:          -%2d / %2d pts  (%s)\n",
                                                        component_scores$outliers$penalty,
                                                        component_scores$outliers$max_penalty,
                                                        component_scores$outliers$description))
            quality_text <- paste0(quality_text, sprintf("\u{2022} Variability:       -%2d / %2d pts  (%s)\n",
                                                        component_scores$variability$penalty,
                                                        component_scores$variability$max_penalty,
                                                        component_scores$variability$description))
            quality_text <- paste0(quality_text, sprintf("\u{2022} Clinical Checks:   -%2d / %2d pts  (%s)\n",
                                                        component_scores$clinical$penalty,
                                                        component_scores$clinical$max_penalty,
                                                        component_scores$clinical$description))
            quality_text <- paste0(quality_text, sprintf("\u{2022} Sample Size:       -%2d / %2d pts  (%s)\n",
                                                        component_scores$sample_size$penalty,
                                                        component_scores$sample_size$max_penalty,
                                                        component_scores$sample_size$description))

            quality_text <- paste0(quality_text, sprintf("                     \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n"))
            quality_text <- paste0(quality_text, sprintf("HEURISTIC GRADE:     %s (%s)\n\n",
                                                        quality_grade, score_band))

            # The clinical component can remove up to 20 points from the headline
            # grade on the strength of hard-coded reference ranges, a unit system
            # that may have been guessed, and pattern-matching on the variable NAME.
            # "Caveats & assumptions" spells this out but is off by default, while
            # clinical checks are on by default - so a user can be handed a lowered
            # grade with nothing to tell them what produced it. Placed after the
            # grade so it does not break up the component breakdown above.
            if (isTRUE(self$options$clinicalValidation) &&
                component_scores$clinical$penalty > 0) {
                clinical_note <- .fmt(
                    .("This component cost {points} points. Plausibility bounds are general-population rules of thumb, not validated reference ranges, so they may not suit paediatric, ICU, oncology or athlete populations. Which checks run is decided by matching the variable NAME, so non-standard naming can skip a check or apply the wrong one, and units were {units}. Confirm each flag against your study protocol before acting on it."),
                    points = component_scores$clinical$penalty,
                    units = .("inferred from the data range"))
                # Wrapping and indentation are applied here, never inside .()
                quality_text <- paste0(quality_text,
                    "  ", .("NOTE ON THE CLINICAL PENALTY"), "\n",
                    paste(strwrap(clinical_note, width = 78, prefix = "  "),
                          collapse = "\n"),
                    "\n\n")
            }
            
            # Variable type and basic characteristics
            var_type_desc <- ifelse(is_numeric, "Numeric/Continuous", 
                                   ifelse(is_categorical, "Categorical/Factor", "Other"))
            quality_text <- paste0(quality_text, sprintf("VARIABLE CHARACTERISTICS:\n"))
            quality_text <- paste0(quality_text, sprintf("\u{2022} Variable Type: %s\n", var_type_desc))
            quality_text <- paste0(quality_text, sprintf("\u{2022} Total Observations: %d\n", n_total))
            quality_text <- paste0(quality_text, sprintf("\u{2022} Complete Cases: %d (%.1f%%)\n", n_complete, 100-missing_pct))
            quality_text <- paste0(quality_text, sprintf("\u{2022} Unique Values: %d (%.1f%% of complete cases)\n\n", n_unique, unique_pct))
            
            # Completeness assessment
            quality_text <- paste0(quality_text, "COMPLETENESS ASSESSMENT:\n")
            quality_text <- paste0(quality_text, sprintf("\u{2022} Missing Data Rate: %.1f%% (%d/%d observations)\n", 
                                                        missing_pct, n_missing, n_total))
            quality_text <- paste0(quality_text, sprintf("\u{2022} Completeness Grade: %s\n", 
                                                        private$.interpretMissing(missing_pct)))
            
            # Add missing pattern information if available
            # Print each detected pattern's DESCRIPTION. This used to print
            # names(missing_patterns)[1], the internal list key, so the reader saw
            # "Missing Pattern: mcar_not_applicable" or "clustering" - and every
            # pattern after the first was silently dropped, which with the MCAR
            # explanation switched on hid the runs-test result entirely.
            if (n_missing > 0 && self$options$showPatterns) {
                missing_patterns <- private$.analyzeMissingPatterns(variable)
                for (pattern_text in missing_patterns) {
                    quality_text <- paste0(quality_text,
                        paste(strwrap(paste0("\u{2022} Missing Pattern: ", pattern_text),
                                      width = 78, exdent = 2),
                              collapse = "\n"),
                        "\n")
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

                skewness <- private$.computeSkewness(clean_var)

                quality_text <- paste0(quality_text, "DISTRIBUTION ASSESSMENT:\n")
                quality_text <- paste0(quality_text, sprintf("\u{2022} Central Tendency: Mean = %.3f, Median = %.3f\n",
                                                            mean_val, median(clean_var)))

                # Show CV or explain suppression
                if (!is.na(cv)) {
                    quality_text <- paste0(quality_text, sprintf("\u{2022} Variability: SD = %.3f, MAD = %.3f, CV = %.1f%%\n",
                                                                sd_val, mad_val, cv))
                } else {
                    quality_text <- paste0(quality_text, sprintf("\u{2022} Variability: SD = %.3f, MAD = %.3f (CV suppressed: |mean| < %.3f)\n",
                                                                sd_val, mad_val, cv_min_mean))
                }

                quality_text <- paste0(quality_text, sprintf("\u{2022} Distribution Shape: %s (skewness = %.2f)\n",
                                                            private$.interpretSkewness(skewness), skewness))
                
                # This line sits inside `is_numeric && n_complete >= 2` and used
                # to have no showOutliers guard at all, so with the Outlier
                # analysis checkbox unticked it printed "None detected (excellent
                # data quality)" about a check that had never run. Even when the
                # check DOES run, no flag is absence of evidence, not evidence of
                # clean data.
                if (!outliers_assessed) {
                    quality_text <- paste0(quality_text, "\u{2022} Outliers: not assessed - ", outlier_skip_reason, "\n")
                } else if (outliers_found > 0) {
                    quality_text <- paste0(quality_text, sprintf("\u{2022} Outliers Detected: %d (%.1f%% of data)\n",
                                                                outliers_found, 100*outlier_rate))
                } else {
                    quality_text <- paste0(quality_text, sprintf(
                        "\u{2022} Outliers: no value flagged by %d or more of the %d methods that ran\n",
                        if (outliers_informative_only) 1L else 2L, private$.outlier_n_methods))
                    quality_text <- paste0(quality_text, "  This does not establish that the variable is free of erroneous values;\n")
                    quality_text <- paste0(quality_text, "  these methods find isolated extreme values, not miscoded values inside the observed range.\n")
                }
                quality_text <- paste0(quality_text, "\n")
            }
            
            # Categorical data assessment
            if (is_categorical && n_complete > 0) {
                categorical_assessment <- private$.analyzeCategoricalQuality(variable)
                quality_text <- paste0(quality_text, "CATEGORICAL DATA ASSESSMENT:\n")
                quality_text <- paste0(quality_text, sprintf("\u{2022} Number of Categories: %d\n", n_unique))
                
                if (!is.null(categorical_assessment)) {
                    if (!is.null(categorical_assessment$balanced)) {
                        quality_text <- paste0(quality_text, "\u{2022} Category Balance: Good\n")
                    } else if (!is.null(categorical_assessment$moderate_imbalance)) {
                        quality_text <- paste0(quality_text, "\u{2022} Category Balance: Moderate imbalance detected\n")
                    } else if (!is.null(categorical_assessment$imbalance)) {
                        quality_text <- paste0(quality_text, "\u{2022} Category Balance: Severe imbalance detected\n")
                    }
                }
                quality_text <- paste0(quality_text, "\n")
            }
            
            # Quality issues summary
            if (length(quality_issues) > 0) {
                quality_text <- paste0(quality_text, "QUALITY CONCERNS IDENTIFIED:\n")
                for (issue in quality_issues) {
                    quality_text <- paste0(quality_text, sprintf("\u{2022} %s\n", stringr::str_to_sentence(issue)))
                }
                quality_text <- paste0(quality_text, "\n")
            }
            
            # Enhanced recommendations based on grade and context
            quality_text <- paste0(quality_text, "RECOMMENDATIONS:\n")
            
            if (quality_grade == "A") {
                quality_text <- paste0(quality_text, "INTERPRETATION: High-Quality Data (by heuristic rules)\n")
                quality_text <- paste0(quality_text, "\u{2022} Data appears suitable for planned analyses\n")
                quality_text <- paste0(quality_text, "\u{2022} Few quality concerns based on automated checks\n")
                quality_text <- paste0(quality_text, "\u{2022} Document this quality assessment in study methods\n")
                quality_text <- paste0(quality_text, "\u{2022} Consider as baseline for quality monitoring\n")

            } else if (quality_grade == "B") {
                quality_text <- paste0(quality_text, "INTERPRETATION: Good Quality with Minor Issues (by heuristic rules)\n")
                quality_text <- paste0(quality_text, "\u{2022} Data likely suitable for analysis with documented limitations\n")
                quality_text <- paste0(quality_text, "\u{2022} Note identified issues in study methods section\n")
                quality_text <- paste0(quality_text, "\u{2022} Consider sensitivity analyses for robust conclusions\n")
                quality_text <- paste0(quality_text, "\u{2022} Monitor quality trends in ongoing data collection\n")

            } else if (quality_grade == "C") {
                quality_text <- paste0(quality_text, "INTERPRETATION: Quality Concerns Detected (by heuristic rules)\n")
                quality_text <- paste0(quality_text, "\u{2022} Data quality issues may affect analysis validity\n")
                quality_text <- paste0(quality_text, "\u{2022} Review specific issues below and consider cleaning\n")
                quality_text <- paste0(quality_text, "\u{2022} Perform sensitivity analyses to assess impact\n")
                quality_text <- paste0(quality_text, "\u{2022} Consult with data management or statistician\n")
                quality_text <- paste0(quality_text, "\u{2022} Document all cleaning decisions and rationale\n")

            } else {
                quality_text <- paste0(quality_text, "INTERPRETATION: Significant Quality Issues (by heuristic rules)\n")
                quality_text <- paste0(quality_text, "\u{2022} Major data quality concerns may threaten validity\n")
                quality_text <- paste0(quality_text, "\u{2022} Caution: Analysis may produce unreliable results\n")
                quality_text <- paste0(quality_text, "\u{2022} Investigate root causes of quality problems\n")
                quality_text <- paste0(quality_text, "\u{2022} Consider whether data can be salvaged or need re-collection\n")
                quality_text <- paste0(quality_text, "\u{2022} Consult with senior investigator before proceeding\n")
            }
            
            # Specific actionable recommendations
            quality_text <- paste0(quality_text, "\nSPECIFIC ACTIONS:\n")
            
            if (missing_pct > 15) {
                quality_text <- paste0(quality_text, "\u{2022} MISSING DATA: Investigate missing data mechanisms (MCAR/MAR/MNAR)\n")
                quality_text <- paste0(quality_text, "\u{2022} MISSING DATA: Consider multiple imputation methods for sensitivity analysis\n")
            }
            
            if (outliers_found > 0) {
                quality_text <- paste0(quality_text, "\u{2022} OUTLIERS: Review each outlier for data entry errors and clinical plausibility\n")
                quality_text <- paste0(quality_text, "\u{2022} OUTLIERS: Consider robust analysis methods (e.g., rank-based tests)\n")
            }

            if (!is.null(clinical_issues_found) && length(clinical_issues_found) > 0) {
                quality_text <- paste0(quality_text, "\u{2022} CLINICAL VALIDATION: Verify measurement units and clinical plausibility\n")
                quality_text <- paste0(quality_text, "\u{2022} CLINICAL VALIDATION: Review data collection procedures\n")
            }
            
            if (n_total < 30) {
                quality_text <- paste0(quality_text, "\u{2022} SAMPLE SIZE: Consider collecting additional data for robust analysis\n")
                quality_text <- paste0(quality_text, "\u{2022} SAMPLE SIZE: Use appropriate methods for small sample sizes\n")
            }
            
            # Add validation warnings if present
            if (length(validation_results$warnings) > 0) {
                quality_text <- paste0(quality_text, "\nVALIDATION WARNINGS:\n")
                for (warning in validation_results$warnings) {
                    quality_text <- paste0(quality_text, sprintf("\u{2022} %s\n", warning))
                }
            }
            
            # Add recommendations if present
            if (length(validation_results$recommendations) > 0) {
                quality_text <- paste0(quality_text, "\nADDITIONAL RECOMMENDATIONS:\n")
                for (rec in validation_results$recommendations) {
                    quality_text <- paste0(quality_text, sprintf("\u{2022} %s\n", rec))
                }
            }
            
            # Add context-specific limitations section
            quality_text <- paste0(quality_text, "\n")
            quality_text <- paste0(quality_text, "LIMITATIONS OF THIS ASSESSMENT:\n\n")

            limitations_added <- FALSE

            # Outlier detection limitations
            if (!outliers_assessed) {
                quality_text <- paste0(quality_text, "\u{2022} OUTLIERS: Not assessed - ", outlier_skip_reason, ".\n")
                quality_text <- paste0(quality_text, "  The Outliers component of the grade above therefore carries no penalty,\n")
                quality_text <- paste0(quality_text, "  and the grade says nothing about extreme values in this variable.\n")
                limitations_added <- TRUE
            } else if (is_numeric && self$options$showOutliers) {
                if (n_complete < 10) {
                    quality_text <- paste0(quality_text, "\u{2022} OUTLIERS (n=", n_complete, "): Informative only, NOT statistically robust.\n")
                    quality_text <- paste0(quality_text, "  Single-method flags shown for early QC; manually verify before taking action.\n")
                    quality_text <- paste0(quality_text, "  These flags do not contribute to the Outliers component of the grade above.\n")
                    limitations_added <- TRUE
                } else if (outliers_found > 0) {
                    transform_applied <- private$.outlier_transform %in% c("log", "sqrt")
                    outlier_transform_note <- if (transform_applied) {
                        paste0(" on ", private$.outlier_transform, "-transformed scale")
                    } else {
                        ""
                    }
                    quality_text <- paste0(quality_text, "\u{2022} OUTLIERS: Consensus detection", outlier_transform_note, "; assumes approximate normality.\n")

                    if (!is.null(skewness) && abs(skewness) > 1 && !transform_applied) {
                        quality_text <- paste0(quality_text, "  WARNING: Severe skewness (", round(skewness, 2), ") without transform may cause Z-score false positives.\n")
                    }
                    limitations_added <- TRUE
                }
            }

            # Missingness limitations
            if (n_missing > 0) {
                quality_text <- paste0(quality_text, "\u{2022} MISSINGNESS: Pattern tests are HEURISTIC")
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
            # clinical_issues_found is assigned unconditionally with Component 4
            # above, so the exists() call here was inert - and, like the skewness
            # one, would have silently resolved to something in the package's
            # imports environment had that ever stopped being true.
            if (self$options$clinicalValidation &&
                !is.null(clinical_issues_found) && length(clinical_issues_found) > 0) {
                quality_text <- paste0(quality_text, "\u{2022} CLINICAL CHECKS: Hard-coded plausibility ranges may not suit all populations.\n")
                quality_text <- paste0(quality_text, "  May over-flag: pediatric, ICU, elite athletes, or diverse ethnic populations.\n")
                quality_text <- paste0(quality_text, "  Units are inferred from the value range; manually verify flagged values.\n")
                limitations_added <- TRUE
            }

            # Quality score limitations (always shown)
            quality_text <- paste0(quality_text, "\u{2022} QUALITY SCORE: Based on ARBITRARY thresholds and penalties, NOT externally validated.\n")
            quality_text <- paste0(quality_text, "  NOT suitable for regulatory submissions or as standalone quality metric.\n")
            quality_text <- paste0(quality_text, "  Use as initial screening tool only; review component breakdown for specific issues.\n")
            limitations_added <- TRUE

            # CV limitations
            if (is_numeric && self$options$showDistribution) {
                cv_min_mean <- self$options$cvMinMean
                quality_text <- paste0(quality_text, "\u{2022} CV CALCULATION: Suppressed when |mean| < ", cv_min_mean, " to avoid instability.\n")
                quality_text <- paste0(quality_text, "  Use MAD or IQR for spread when CV not reported. CV only appropriate for ratio-scale data.\n")
                limitations_added <- TRUE
            }

            # Missingness-mechanism limitation. This is the one place the MCAR
            # explanation is guaranteed to reach the user. Its only other consumer
            # is .analyzeMissingPatterns, called from two sites that are both
            # gated on showPatterns - a second, unrelated checkbox that is off by
            # default - so out of the box, ticking "Explain MCAR testability"
            # changed no table row and no text anywhere.
            if (isTRUE(self$options$mcarTest)) {
                quality_text <- paste0(quality_text, "\u{2022} MCAR: Little's MCAR test is multivariate and cannot be computed from a single\n")
                quality_text <- paste0(quality_text, "  variable - it compares means across missingness patterns using the OTHER variables\n")
                quality_text <- paste0(quality_text, "  in the dataset. The runs and dropout results reported here are heuristics about\n")
                quality_text <- paste0(quality_text, "  WHERE the missing values sit, not a test of the missingness mechanism. To test\n")
                quality_text <- paste0(quality_text, "  MCAR formally, run naniar::mcar_test() on the full dataset.\n")
                limitations_added <- TRUE
            }

            # General limitation footer
            if (limitations_added) {
                quality_text <- paste0(quality_text, "\nCRITICAL REMINDER: This is an automated SCREENING tool to identify potential issues.\n")
                quality_text <- paste0(quality_text, "   Final data quality decisions MUST incorporate:\n")
                quality_text <- paste0(quality_text, "   - Clinical/domain expertise for context\n")
                quality_text <- paste0(quality_text, "   - Manual verification of flagged values\n")
                quality_text <- paste0(quality_text, "   - Statistical judgment for analysis planning\n")
                quality_text <- paste0(quality_text, "   - Study-specific quality requirements\n")
            }

            quality_text <- paste0(quality_text, "\n\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\n")
            quality_text <- paste0(quality_text, "Generated by ClinicoPath Data Quality Assessment Module\n")
            quality_text <- paste0(quality_text, sprintf("Assessment Date: %s\n", Sys.Date()))
            quality_text <- paste0(quality_text, "\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}")

            self$results$qualityText$setContent(quality_text)

            # ========== EDUCATIONAL PANELS ==========

            # Natural-Language Summary (for copying to reports)
            if (self$options$showSummary) {
                summary_html <- "<div style='font-family: Georgia, serif; line-height: 1.8; padding: 15px; background-color: rgba(155, 155, 155, 0.06); border-left: 4px solid #2c5aa0; color: inherit;'>"
                summary_html <- paste0(summary_html, "<h3 style='margin-top: 0;'>Data Quality Summary</h3>")
                summary_html <- paste0(summary_html, "<p><strong>Variable:</strong> ", htmltools::htmlEscape(var_name), "</p>")
                summary_html <- paste0(summary_html, "<p><strong>Overall Quality Grade:</strong> ", quality_grade, " (", max(0, min(100, quality_score)), "/100 by heuristic scoring)</p>")

                # Sample characteristics
                if (is_numeric) {
                    summary_html <- paste0(summary_html, sprintf("<p>This numeric variable contains <strong>%d observations</strong> with <strong>%.1f%% missing data</strong> (%d/%d cases). ", n_total, missing_pct, n_missing, n_total))
                } else {
                    summary_html <- paste0(summary_html, sprintf("<p>This categorical variable contains <strong>%d observations</strong> across <strong>%d unique categories</strong> with <strong>%.1f%% missing data</strong> (%d/%d cases). ", n_total, length(unique(variable[!is.na(variable)])), missing_pct, n_missing, n_total))
                }

                # Key findings
                # "Consensus" only below n = 10 is untrue: there the rule is a
                # single method, and the flags are excluded from the grade.
                if (outliers_found > 0) {
                    outlier_basis <- if (outliers_informative_only)
                        "Informative-only outlier screening (single-method flags, n<10, not scored in the grade)"
                    else
                        "Consensus outlier detection"
                    summary_html <- paste0(summary_html, sprintf("%s identified <strong>%d potential outliers</strong> (%.1f%% of non-missing cases). ", outlier_basis, outliers_found, (outliers_found/n_complete)*100))
                }

                # One entry per CHECK that failed, never one per observation:
                # .clinicalContextValidation returns at most one issue per rule,
                # so an Age column with 40 values above 120 and 3 negative values
                # produced "flagged 2 observations". A reader pasting this panel
                # into a methods section would have published a false count.
                if (length(penalizable_clinical) > 0) {
                    summary_html <- paste0(summary_html, sprintf("Clinical plausibility checks raised <strong>%d</strong> flag(s) - one per check that failed, not one per value. ", length(penalizable_clinical)))
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

                summary_html <- paste0(summary_html, "<p style='font-size: 0.9em; opacity: 0.75; margin-top: 15px;'><em>Note: This assessment uses heuristic quality rules and should be combined with clinical/domain expertise for final data quality decisions.</em></p>")
                summary_html <- paste0(summary_html, "</div>")

                self$results$naturalSummary$setContent(summary_html)
            }

            # About This Analysis panel
            if (self$options$showAbout) {
                about_html <- "<div style='font-family: Arial, sans-serif; line-height: 1.6; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4682b4; color: inherit;'>"
                about_html <- paste0(about_html, "<h3 style='color: #4682b4; margin-top: 0;'>About Data Quality Assessment</h3>")

                about_html <- paste0(about_html, "<h4>Purpose</h4>")
                about_html <- paste0(about_html, "<p>This analysis performs comprehensive quality assessment for single variables to identify potential data issues before statistical analysis. It helps researchers detect missing data patterns, outliers, clinical implausibility, and other quality concerns that may affect study validity.</p>")

                about_html <- paste0(about_html, "<h4>Assessment Components</h4>")
                about_html <- paste0(about_html, "<ul>")
                about_html <- paste0(about_html, "<li><strong>Missing Data Analysis:</strong> Examines completeness, missing data patterns, and heuristic assessment of potential mechanisms (MCAR/MAR/MNAR) using runs test when sample size permits</li>")
                about_html <- paste0(about_html, "<li><strong>Outlier Detection:</strong> Uses consensus approach requiring agreement from \u{2265}2 methods (Z-score |z|>3, IQR 1.5\u{D7}rule, Modified Z-score MAD-based |z|>3.5) to minimize false positives</li>")
                about_html <- paste0(about_html, "<li><strong>Distribution Analysis:</strong> Provides descriptive statistics, robust spread (MAD, IQR), coefficient of variation and the moment coefficient of skewness for numeric variables. No normality test is computed</li>")
                about_html <- paste0(about_html, "<li><strong>Clinical Validation:</strong> Applies hard-coded plausibility ranges for common clinical variables (age, vital signs, lab values); units are inferred from the value range</li>")
                about_html <- paste0(about_html, "<li><strong>Quality Scoring:</strong> Generates heuristic composite score (0-100) based on completeness, outlier prevalence, sample size, and variability</li>")
                about_html <- paste0(about_html, "</ul>")

                about_html <- paste0(about_html, "<h4>Quality Grade Interpretation</h4>")
                about_html <- paste0(about_html, "<ul>")
                about_html <- paste0(about_html, "<li><strong>Grade A (90-100):</strong> Excellent quality - minimal issues, suitable for standard analysis</li>")
                about_html <- paste0(about_html, "<li><strong>Grade B (80-89):</strong> Good quality - minor issues requiring documentation but analysis can proceed</li>")
                about_html <- paste0(about_html, "<li><strong>Grade C (70-79):</strong> Quality concerns - significant issues requiring review and sensitivity analyses</li>")
                about_html <- paste0(about_html, "<li><strong>Grade D (<70):</strong> Poor quality - major validity threats, consider data cleaning or re-collection</li>")
                about_html <- paste0(about_html, "</ul>")

                about_html <- paste0(about_html, "<h4>Advanced Options</h4>")
                about_html <- paste0(about_html, "<ul>")
                about_html <- paste0(about_html, "<li><strong>Outlier Transformation:</strong> Apply log or square root transformations before outlier detection for right-skewed distributions (common in lab values)</li>")
                about_html <- paste0(about_html, "<li><strong>Explain MCAR testability:</strong> States why the missingness mechanism cannot be tested formally from a single variable. Little's MCAR test is multivariate, and the runs and dropout results reported here are heuristics about where the missing values sit, not a test of the mechanism. No formal MCAR test is performed by this analysis</li>")
                about_html <- paste0(about_html, "<li><strong>Rare Category Threshold:</strong> Flag categories occurring in <X% of observations (important for chi-squared test assumptions and modeling stability)</li>")
                about_html <- paste0(about_html, "</ul>")

                about_html <- paste0(about_html, "<p style='font-size: 0.9em; opacity: 0.75; margin-top: 15px;'><em>For detailed methodology and validation studies, see ClinicoPath module documentation at <a href='https://www.serdarbalci.com/ClinicoPathDescriptives/' target='_blank'>https://www.serdarbalci.com/ClinicoPathDescriptives/</a></em></p>")
                about_html <- paste0(about_html, "</div>")

                self$results$aboutAnalysis$setContent(about_html)
            }

            # Caveats & Assumptions panel
            if (self$options$showCaveats) {
                caveats_html <- "<div style='font-family: Arial, sans-serif; line-height: 1.6; padding: 15px; background-color: rgba(255, 211, 33, 0.16); border-left: 4px solid #ffa500; color: inherit;'>"
                caveats_html <- paste0(caveats_html, "<h3 style='color: #d2691e; margin-top: 0;'>Important Caveats &amp; Assumptions</h3>")

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
                caveats_html <- paste0(caveats_html, "<li><strong>Mixed scales when a transform is applied:</strong> With a log or square-root transform, flagged <em>values</em> are shown on the original scale while the z-scores and the IQR fence are computed and reported on the transformed scale. Do not compare a reported bound directly against a reported value; the severity label states which scale it used</li>")
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
                caveats_html <- paste0(caveats_html, "<li><strong>Unit inference is heuristic:</strong> Units are inferred from the value range and may be misclassified in edge cases (e.g., height in metres vs cm, creatinine in mg/dL vs \u{B5}mol/L); verify the units of flagged values</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Variable name matching:</strong> Clinical checks use pattern matching on variable names (e.g., 'age', 'glucose', 'systolic'); may miss or misclassify non-standard naming</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>Context-specific ranges:</strong> Normal ranges vary by measurement method, population demographics, and clinical context; verify against study-specific protocols</li>")
                caveats_html <- paste0(caveats_html, "</ul>")

                caveats_html <- paste0(caveats_html, "<h4>Statistical Assumptions</h4>")
                caveats_html <- paste0(caveats_html, "<ul>")
                caveats_html <- paste0(caveats_html, "<li><strong>Independence assumption:</strong> Outlier detection assumes independent observations; this may be violated in clustered or longitudinal data</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>CV calculation:</strong> Coefficient of variation only appropriate for ratio-scale data with meaningful zero; suppressed when |mean| < threshold to avoid instability</li>")
                caveats_html <- paste0(caveats_html, "<li><strong>No normality test is performed:</strong> Distribution shape is summarised by the moment coefficient of skewness with rule-of-thumb bands (|skew| &lt; 0.5 approximately symmetric, &lt; 1 moderately skewed, otherwise highly skewed). These bands are descriptive, not a test, and a value inside a band is not evidence that the distribution is normal. Use Q-Q plots, a formal test elsewhere in jamovi, and domain knowledge when normality actually matters</li>")
                caveats_html <- paste0(caveats_html, "</ul>")

                caveats_html <- paste0(caveats_html, "<h4>Recommended Workflow</h4>")
                caveats_html <- paste0(caveats_html, "<p style='background-color: rgba(255, 255, 255, 0.06); padding: 10px; border-left: 3px solid #ffa500; color: inherit;'>")
                caveats_html <- paste0(caveats_html, "<strong>Step 1:</strong> Use this tool for initial automated screening<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 2:</strong> Manually verify all flagged observations with clinical/domain expertise<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 3:</strong> Investigate root causes (data entry errors, measurement issues, true biological variation)<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 4:</strong> Document all quality decisions and cleaning actions with justification<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 5:</strong> Perform sensitivity analyses comparing results with/without flagged observations<br>")
                caveats_html <- paste0(caveats_html, "<strong>Step 6:</strong> Report quality assessment and handling in study methods section")
                caveats_html <- paste0(caveats_html, "</p>")

                caveats_html <- paste0(caveats_html, "<p style='font-size: 0.9em; color: #d2691e; margin-top: 15px; font-weight: bold;'>")
                caveats_html <- paste0(caveats_html, "CRITICAL: Automated quality assessment is a starting point, not a substitute for statistical and clinical judgment. Always combine algorithmic screening with expert review before making data cleaning decisions.")
                caveats_html <- paste0(caveats_html, "</p>")

                caveats_html <- paste0(caveats_html, "</div>")

                self$results$caveatsAssumptions$setContent(caveats_html)
            }
        }
    )
)
