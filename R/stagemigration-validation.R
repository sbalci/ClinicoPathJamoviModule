# Stage Migration Analysis - Data Validation Functions
#
# Comprehensive data validation, cleaning, and preparation functions for
# stage migration analysis.

# =============================================================================
# DATA VALIDATION AND PREPARATION
# =============================================================================

#' Validate and Prepare Analysis Data
#'
#' @description
#' Master data validation function that performs comprehensive checks and
#' preparations for stage migration analysis.
#'
#' @param data Raw input data frame
#' @param options Analysis options list containing variable names and settings
#' @param additional_vars Optional character vector of further columns the
#'   analysis needs (multifactorial covariates, the institution variable). They
#'   are checked for existence and included in the complete-case filter, so a
#'   covariate with missing values cannot silently drop rows later, inside the
#'   model fit, where the loss would go unreported.
#' @param checkpoint_callback Optional zero-argument function called between the
#'   expensive validation stages so jamovi can stay responsive.
#' @return List with validated data, warnings, errors, and metadata
#' @keywords internal
# NOTE: `additional_vars` and `checkpoint_callback` are not new. R/stagemigration.b.R
# has always passed them, while this signature accepted only (data, options), so
# every call raised "unused arguments (additional_vars = ..., checkpoint_callback
# = ...)" and the analysis could not run at all. Implementing them is what the
# caller intended; dropping them from the call would have discarded the covariate
# and institution checks the caller carefully assembles.
#
# Plain `#` comments, not `#'`: roxygen consumes `#'` lines following a tag as
# that tag's VALUE, which is what produced "@keywords must be only 1 line long".
stagemigration_validateData <- function(data, options,
                                        additional_vars = NULL,
                                        checkpoint_callback = NULL) {
    # Tolerate any of NULL / "" / NA in the supplied extras.
    additional_vars <- unique(additional_vars[
        !is.null(additional_vars) & !is.na(additional_vars) & nzchar(additional_vars)])
    .cp <- function() if (is.function(checkpoint_callback))
        try(checkpoint_callback(), silent = TRUE) else invisible(NULL)
    validation_result <- list(
        data = data,
        valid = FALSE,
        errors = character(0),
        warnings = character(0),
        notices = character(0),
        metadata = list()
    )

    # Extract variable names from options
    old_stage <- options$oldStage
    new_stage <- options$newStage
    time_var <- options$survivalTime
    event_var <- options$event
    event_level <- options$eventLevel

    # Check required variables are specified
    if (is.null(old_stage) || old_stage == "") {
        validation_result$errors <- c(validation_result$errors,
            "Old stage variable not specified")
        return(validation_result)
    }
    if (is.null(new_stage) || new_stage == "") {
        validation_result$errors <- c(validation_result$errors,
            "New stage variable not specified")
        return(validation_result)
    }
    if (is.null(time_var) || time_var == "") {
        validation_result$errors <- c(validation_result$errors,
            "Survival time variable not specified")
        return(validation_result)
    }
    if (is.null(event_var) || event_var == "") {
        validation_result$errors <- c(validation_result$errors,
            "Event variable not specified")
        return(validation_result)
    }

    # Check variables exist in data
    required_vars <- c(old_stage, new_stage, time_var, event_var, additional_vars)
    missing_vars <- required_vars[!required_vars %in% names(data)]

    if (length(missing_vars) > 0) {
        validation_result$errors <- c(validation_result$errors,
            paste("Variables not found in data:",
                  paste(missing_vars, collapse = ", ")))
        return(validation_result)
    }

    # Store original data dimensions
    validation_result$metadata$n_original <- nrow(data)

    # =========================================================================
    # STEP 1: Handle labelled data (SPSS/Stata/SAS imports)
    # =========================================================================

    staging_vars <- c(old_stage, new_stage)
    data <- stagemigration_convertLabelled(data, staging_vars, verbose = FALSE)

    # Also handle categorical covariates if present
    if (isTRUE(options$enableMultifactorialAnalysis)) {
        cat_covars <- options$categoricalCovariates
        if (!is.null(cat_covars) && length(cat_covars) > 0) {
            existing_covars <- cat_covars[cat_covars %in% names(data)]
            if (length(existing_covars) > 0) {
                data <- stagemigration_convertLabelled(data, existing_covars, verbose = FALSE)
            }
        }
    }

    # =========================================================================
    # STEP 2: Validate and prepare staging variables
    # =========================================================================

    stage_validation <- stagemigration_validateStagingVars(
        data, old_stage, new_stage, verbose = FALSE
    )

    # Append staging validation results
    validation_result$errors <- c(validation_result$errors, stage_validation$errors)
    validation_result$warnings <- c(validation_result$warnings, stage_validation$warnings)

    if (length(stage_validation$errors) > 0) {
        return(validation_result)
    }

    # Use validated data
    data <- stage_validation$data
    validation_result$metadata$old_stage_levels <- stage_validation$old_stage_levels
    validation_result$metadata$new_stage_levels <- stage_validation$new_stage_levels

    # =========================================================================
    # STEP 3: Validate and prepare survival variables
    # =========================================================================

    # Validate survival time
    if (!is.numeric(data[[time_var]])) {
        validation_result$errors <- c(validation_result$errors,
            paste("Survival time variable must be numeric, found:",
                  class(data[[time_var]])[1]))
        return(validation_result)
    }

    # Check for negative survival times
    if (any(data[[time_var]] < 0, na.rm = TRUE)) {
        validation_result$errors <- c(validation_result$errors,
            "Survival time contains negative values")
        return(validation_result)
    }

    # Check for zero survival times (warning only)
    n_zero_time <- sum(data[[time_var]] == 0, na.rm = TRUE)
    if (n_zero_time > 0) {
        validation_result$warnings <- c(validation_result$warnings,
            paste(n_zero_time, "patients with zero follow-up time"))
    }

    # Create binary event indicator
    event_binary <- stagemigration_createEventBinary(
        data[[event_var]], event_level
    )

    if (is.null(event_binary$binary)) {
        validation_result$errors <- c(validation_result$errors,
            event_binary$error)
        return(validation_result)
    }

    data$event_binary <- event_binary$binary
    validation_result$metadata$event_level_used <- event_binary$level_used
    validation_result$notices <- c(validation_result$notices,
        paste("Event level used:", event_binary$level_used))

    # =========================================================================
    # STEP 4: Remove missing values and validate completeness
    # =========================================================================

    # Check missing data before removal
    .cp()
    # Covariates join the complete-case filter here rather than being dropped
    # later inside coxph(), where the row loss is invisible to the user.
    required_for_complete <- c(old_stage, new_stage, time_var, "event_binary",
                               additional_vars)

    missing_summary <- list()
    for (var in required_for_complete) {
        n_missing <- sum(is.na(data[[var]]))
        pct_missing <- (n_missing / nrow(data)) * 100

        if (n_missing > 0) {
            missing_summary[[var]] <- list(
                n = n_missing,
                pct = pct_missing
            )

            validation_result$warnings <- c(validation_result$warnings,
                sprintf("%s: %d missing values (%.1f%%)", var, n_missing, pct_missing))
        }
    }

    # Remove incomplete cases
    complete_cases <- complete.cases(data[, required_for_complete, drop = FALSE])
    n_incomplete <- sum(!complete_cases)

    if (n_incomplete > 0) {
        validation_result$notices <- c(validation_result$notices,
            paste("Removed", n_incomplete, "incomplete cases"))
        data <- data[complete_cases, ]
    }

    validation_result$metadata$n_after_missing <- nrow(data)
    validation_result$metadata$additional_vars <- additional_vars
    .cp()

    # Check if enough data remains
    if (nrow(data) < 10) {
        validation_result$errors <- c(validation_result$errors,
            paste("Insufficient data after removing missing values:",
                  nrow(data), "cases remaining"))
        return(validation_result)
    }

    # =========================================================================
    # STEP 5: Validate sample size and event rates
    # =========================================================================

    n_events <- sum(data$event_binary == 1)
    n_total <- nrow(data)

    # Events-per-variable must count the degrees of freedom the Cox models actually spend.
    # This was hardcoded to 2, but a stage factor with k levels uses k - 1 df, and each staging
    # system is fitted as its own model -- so a 4-stage system spends 3 df, and multifactorial
    # covariates add more. Undercounting overstated EPV and silenced the warning in exactly the
    # small cohorts where it matters. Use the more demanding of the two models.
    stage_df <- function(v) {
        if (is.null(v) || !nzchar(v) || !(v %in% names(data))) return(0L)
        max(1L, nlevels(droplevels(factor(data[[v]]))) - 1L)
    }
    n_predictors <- max(stage_df(old_stage), stage_df(new_stage), 1L)
    if (isTRUE(options$enableMultifactorialAnalysis)) {
        cont <- options$continuousCovariates
        cont <- cont[!is.null(cont) & nzchar(cont) & cont %in% names(data)]
        cats <- options$categoricalCovariates
        cats <- cats[!is.null(cats) & nzchar(cats) & cats %in% names(data)]
        n_predictors <- n_predictors + length(cont) +
            sum(vapply(cats, function(v) max(1L, nlevels(droplevels(factor(data[[v]]))) - 1L), integer(1)))
    }
    validation_result$metadata$n_predictors_df <- n_predictors

    sample_check <- stagemigration_checkSampleSize(
        n = n_total,
        n_events = n_events,
        n_predictors = n_predictors,
        analysis_type = options$analysisType %||% "standard"
    )

    validation_result$metadata$sample_adequacy <- sample_check

    # Add sample size messages
    validation_result$warnings <- c(validation_result$warnings, sample_check$messages)
    if (length(sample_check$recommendations) > 0) {
        validation_result$notices <- c(validation_result$notices,
            sample_check$recommendations)
    }

    # Critical: block analysis if sample too small
    if (sample_check$level == "CRITICAL") {
        # State the actual shortfall: an all-censored cohort of 700 patients used to be told its
        # "sample size" was too small, which points the user at the wrong problem.
        validation_result$errors <- c(validation_result$errors,
            sprintf("Too few events for a meaningful analysis: %d events among %d patients; at least 10 events are needed.",
                    as.integer(n_events), as.integer(n_total)))
        return(validation_result)
    }

    # Check for sparse staging subgroups (<5 patients) and zero-event categories
    if (!is.null(old_stage) && old_stage %in% names(data)) {
        old_counts <- table(data[[old_stage]])
        old_sparse <- names(old_counts)[old_counts > 0 & old_counts < 5]
        if (length(old_sparse) > 0) {
            validation_result$warnings <- c(validation_result$warnings,
                sprintf("Original staging contains sparse category (<5 patients): %s. Parameter estimates for this stage may have high standard errors.",
                        paste(old_sparse, collapse = ", ")))
        }
        if ("event_binary" %in% names(data)) {
            old_events <- tapply(data$event_binary, data[[old_stage]], sum, na.rm = TRUE)
            old_zero_ev <- names(old_events)[!is.na(old_events) & old_events == 0]
            if (length(old_zero_ev) > 0) {
                validation_result$warnings <- c(validation_result$warnings,
                    sprintf("Original staging contains category with 0 events: %s. This may cause model convergence issues or infinite hazard ratios.",
                            paste(old_zero_ev, collapse = ", ")))
            }
        }
    }

    if (!is.null(new_stage) && new_stage %in% names(data)) {
        new_counts <- table(data[[new_stage]])
        new_sparse <- names(new_counts)[new_counts > 0 & new_counts < 5]
        if (length(new_sparse) > 0) {
            validation_result$warnings <- c(validation_result$warnings,
                sprintf("New staging contains sparse category (<5 patients): %s. Parameter estimates for this stage may have high standard errors.",
                        paste(new_sparse, collapse = ", ")))
        }
        if ("event_binary" %in% names(data)) {
            new_events <- tapply(data$event_binary, data[[new_stage]], sum, na.rm = TRUE)
            new_zero_ev <- names(new_events)[!is.na(new_events) & new_events == 0]
            if (length(new_zero_ev) > 0) {
                validation_result$warnings <- c(validation_result$warnings,
                    sprintf("New staging contains category with 0 events: %s. This may cause model convergence issues or infinite hazard ratios.",
                            paste(new_zero_ev, collapse = ", ")))
            }
        }
    }

    # =========================================================================
    # STEP 6: Validate covariates if multifactorial analysis enabled
    # =========================================================================

    if (isTRUE(options$enableMultifactorialAnalysis)) {
        covariate_validation <- stagemigration_validateCovariates(
            data, options
        )

        validation_result$warnings <- c(validation_result$warnings,
            covariate_validation$warnings)
        validation_result$notices <- c(validation_result$notices,
            covariate_validation$notices)

        validation_result$metadata$covariates_valid <- covariate_validation$valid
        validation_result$metadata$continuous_covariates <- covariate_validation$continuous
        validation_result$metadata$categorical_covariates <- covariate_validation$categorical
    }

    # =========================================================================
    # STEP 7: Run data quality report
    # =========================================================================

    quality_report <- stagemigration_dataQualityReport(
        data, old_stage, new_stage, time_var, "event_binary"
    )

    validation_result$metadata$data_quality <- quality_report

    if (quality_report$overall_quality == "INADEQUATE") {
        validation_result$errors <- c(validation_result$errors,
            "Data quality inadequate for analysis")
        validation_result$errors <- c(validation_result$errors,
            quality_report$recommendations)
        return(validation_result)
    }

    if (quality_report$overall_quality == "POOR") {
        validation_result$warnings <- c(validation_result$warnings,
            "Data quality is poor - interpret results with caution")
    }

    # =========================================================================
    # FINAL: Return validated data
    # =========================================================================

    validation_result$data <- data
    validation_result$valid <- TRUE

    return(validation_result)
}

#' Create Binary Event Indicator
#'
#' @description
#' Converts event variable to binary (0/1) format, handling factors and numeric.
#'
#' @param event_var Event variable (factor or numeric)
#' @param event_level Level that indicates event occurred
#' @return List with binary indicator and metadata
#' @keywords internal
stagemigration_createEventBinary <- function(event_var, event_level = NULL) {
    result <- list(
        binary = NULL,
        level_used = NULL,
        error = NULL
    )

    # If already binary numeric
    if (is.numeric(event_var)) {
        unique_vals <- unique(event_var[!is.na(event_var)])

        if (all(unique_vals %in% c(0, 1))) {
            result$binary <- event_var
            result$level_used <- "1"
            return(result)
        }

        # Not binary - try to convert
        if (!is.null(event_level)) {
            result$binary <- as.numeric(event_var == event_level)
            result$level_used <- as.character(event_level)
            return(result)
        } else {
            result$error <- "Event variable is numeric but not binary (0/1). Specify eventLevel."
            return(result)
        }
    }

    # If factor or character
    if (is.factor(event_var) || is.character(event_var)) {
        if (is.character(event_var)) {
            event_var <- factor(event_var)
        }

        levels_present <- levels(event_var)

        # If event_level specified, use it
        if (!is.null(event_level) && event_level != "") {
            if (event_level %in% levels_present) {
                result$binary <- as.numeric(event_var == event_level)
                result$level_used <- event_level
                return(result)
            } else {
                result$error <- paste("Specified event level not found:",
                                     event_level,
                                     ". Available levels:",
                                     paste(levels_present, collapse = ", "))
                return(result)
            }
        }

        # Auto-detect event level (case-insensitive common terms)
        event_keywords <- c("dead", "death", "died", "event", "yes", "true", "1")

        for (level in levels_present) {
            if (tolower(level) %in% event_keywords) {
                result$binary <- as.numeric(event_var == level)
                result$level_used <- level
                return(result)
            }
        }

        # If only 2 levels, use second level as event
        if (length(levels_present) == 2) {
            result$binary <- as.numeric(as.numeric(event_var) - 1)
            result$level_used <- levels_present[2]
            return(result)
        }

        result$error <- paste("Cannot auto-detect event level. Please specify eventLevel.",
                             "Available levels:",
                             paste(levels_present, collapse = ", "))
        return(result)
    }

    result$error <- paste("Event variable type not supported:",
                         class(event_var)[1])
    return(result)
}

#' Validate Covariates for Multifactorial Analysis
#'
#' @description
#' Validates continuous and categorical covariates for inclusion in
#' multifactorial models.
#'
#' @param data Data frame
#' @param options Analysis options
#' @return List with validation results
#' @keywords internal
stagemigration_validateCovariates <- function(data, options) {
    result <- list(
        valid = TRUE,
        continuous = character(0),
        categorical = character(0),
        warnings = character(0),
        notices = character(0)
    )

    # Validate continuous covariates
    cont_covars <- options$continuousCovariates
    if (!is.null(cont_covars) && length(cont_covars) > 0) {
        for (var in cont_covars) {
            if (!var %in% names(data)) {
                result$warnings <- c(result$warnings,
                    paste("Continuous covariate not found:", var))
                next
            }

            if (!is.numeric(data[[var]])) {
                result$warnings <- c(result$warnings,
                    paste("Continuous covariate is not numeric:", var))
                next
            }

            # Check for constant values
            if (length(unique(data[[var]][!is.na(data[[var]])])) < 2) {
                result$warnings <- c(result$warnings,
                    paste("Continuous covariate has no variation:", var))
                next
            }

            result$continuous <- c(result$continuous, var)
        }
    }

    # Validate categorical covariates
    cat_covars <- options$categoricalCovariates
    if (!is.null(cat_covars) && length(cat_covars) > 0) {
        for (var in cat_covars) {
            if (!var %in% names(data)) {
                result$warnings <- c(result$warnings,
                    paste("Categorical covariate not found:", var))
                next
            }

            # Ensure factor
            if (!is.factor(data[[var]])) {
                data[[var]] <- as.factor(data[[var]])
            }

            # Check for adequate levels
            n_levels <- length(levels(data[[var]]))
            if (n_levels < 2) {
                result$warnings <- c(result$warnings,
                    paste("Categorical covariate has < 2 levels:", var))
                next
            }

            if (n_levels > 10) {
                result$warnings <- c(result$warnings,
                    paste("Categorical covariate has many levels (", n_levels, "):", var))
            }

            result$categorical <- c(result$categorical, var)
        }
    }

    # Check total number of predictors vs sample size
    n_predictors <- 2 + length(result$continuous) + length(result$categorical)
    n_events <- sum(data$event_binary == 1, na.rm = TRUE)
    epv <- n_events / n_predictors

    if (epv < STAGEMIGRATION_CONSTANTS$MIN_EPV) {
        result$warnings <- c(result$warnings,
            sprintf("Events per variable (%.1f) below recommended minimum (%d)",
                    epv, STAGEMIGRATION_CONSTANTS$MIN_EPV))
        result$notices <- c(result$notices,
            "Consider reducing number of covariates or collecting more data")
    }

    return(result)
}

#' Outlier Detection for Survival Times
#'
#' @description
#' Identifies potential outliers in survival times using IQR method.
#'
#' @param time_var Numeric vector of survival times
#' @param method Method for outlier detection ("iqr", "zscore")
#' @param threshold Threshold multiplier (default 3.0 for IQR, 3.0 for z-score)
#' @return List with outlier indices and summary
#' @keywords internal
stagemigration_detectOutliers <- function(time_var, method = "iqr", threshold = 3.0) {
    result <- list(
        outlier_indices = integer(0),
        n_outliers = 0,
        outlier_values = numeric(0),
        lower_bound = NA,
        upper_bound = NA,
        method = method
    )

    # Remove missing values
    time_clean <- time_var[!is.na(time_var)]

    if (length(time_clean) < 4) {
        return(result)  # Not enough data for outlier detection
    }

    if (method == "iqr") {
        # IQR method (Tukey's fences)
        q1 <- quantile(time_clean, 0.25)
        q3 <- quantile(time_clean, 0.75)
        iqr <- q3 - q1

        result$lower_bound <- q1 - threshold * iqr
        result$upper_bound <- q3 + threshold * iqr

        outliers <- which(time_var < result$lower_bound | time_var > result$upper_bound)

    } else if (method == "zscore") {
        # Z-score method
        mean_time <- mean(time_clean)
        sd_time <- sd(time_clean)

        result$lower_bound <- mean_time - threshold * sd_time
        result$upper_bound <- mean_time + threshold * sd_time

        z_scores <- abs((time_var - mean_time) / sd_time)
        outliers <- which(z_scores > threshold)
    } else {
        return(result)
    }

    result$outlier_indices <- outliers
    result$n_outliers <- length(outliers)
    if (result$n_outliers > 0) {
        result$outlier_values <- time_var[outliers]
    }

    return(result)
}
