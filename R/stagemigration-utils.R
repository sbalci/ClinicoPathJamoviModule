# Stage Migration Analysis - Core Utility Functions
#
# Core utility functions for consistent variable handling, safe execution,
# and common operations across stage migration analysis modules.

# =============================================================================
# VARIABLE HANDLING UTILITIES
# =============================================================================

#' Escape Variable Names for Safe Handling
#'
#' @description
#' Ensures variable names are safe for use in formulas and data operations.
#' Handles spaces, special characters, and reserved words.
#'
#' @param varname Character string or vector of variable names
#' @param backticks Logical, whether to wrap in backticks (default TRUE)
#' @return Escaped variable name(s)
#' @keywords internal
stagemigration_escapeVar <- function(varname, backticks = TRUE) {
    if (is.null(varname) || length(varname) == 0) {
        return(character(0))
    }

    # Make syntactically valid names
    escaped <- make.names(varname, unique = TRUE)

    # Additional cleaning for special cases
    escaped <- gsub("[^A-Za-z0-9_.]", "_", escaped)

    # Wrap in backticks for formula safety
    if (backticks) {
        escaped <- paste0("`", escaped, "`")
    }

    return(escaped)
}

#' Convert Labelled Data to Factors
#'
#' @description
#' Handles haven::labelled vectors from SPSS/Stata/SAS files by converting
#' them to proper R factors with labels.
#'
#' @param data Data frame
#' @param vars Character vector of variable names to convert
#' @param verbose Logical, whether to print conversion messages
#' @return Data frame with labelled variables converted to factors
#' @keywords internal
stagemigration_convertLabelled <- function(data, vars, verbose = FALSE) {
    if (!requireNamespace("haven", quietly = TRUE)) {
        warning("Package 'haven' not available. Labelled data conversion skipped.")
        return(data)
    }

    for (var in vars) {
        if (!var %in% names(data)) next

        # Check if variable is labelled
        if (inherits(data[[var]], "haven_labelled")) {
            if (verbose) {
                message("Converting labelled variable: ", var)
            }

            # Convert to factor, preserving both labels and values
            data[[var]] <- haven::as_factor(data[[var]], levels = "both")

        } else if (inherits(data[[var]], c("labelled", "labelled_spss"))) {
            # Handle other labelled types
            if (verbose) {
                message("Converting labelled variable: ", var)
            }
            data[[var]] <- haven::as_factor(data[[var]])
        }

        # Ensure factor type even if not labelled
        if (!is.factor(data[[var]]) && !is.numeric(data[[var]])) {
            data[[var]] <- as.factor(data[[var]])
        }
    }

    return(data)
}

#' Validate and Prepare Staging Variables
#'
#' @description
#' Comprehensive validation of staging variables including:
#' - Labelled data conversion
#' - Factor level validation
#' - Missing value checks
#' - Special character handling
#'
#' @param data Data frame
#' @param old_stage_var Name of old staging variable
#' @param new_stage_var Name of new staging variable
#' @param verbose Logical, whether to print validation messages
#' @return List with validated data and metadata
#' @keywords internal
stagemigration_validateStagingVars <- function(data, old_stage_var, new_stage_var, verbose = FALSE) {
    result <- list(
        data = data,
        old_stage_var = old_stage_var,
        new_stage_var = new_stage_var,
        old_stage_levels = NULL,
        new_stage_levels = NULL,
        warnings = character(0),
        errors = character(0)
    )

    # Check variables exist
    if (!old_stage_var %in% names(data)) {
        result$errors <- c(result$errors, paste("Old stage variable not found:", old_stage_var))
        return(result)
    }
    if (!new_stage_var %in% names(data)) {
        result$errors <- c(result$errors, paste("New stage variable not found:", new_stage_var))
        return(result)
    }

    # Convert labelled data
    data <- stagemigration_convertLabelled(data, c(old_stage_var, new_stage_var), verbose)

    # Ensure factor type
    if (!is.factor(data[[old_stage_var]])) {
        data[[old_stage_var]] <- as.factor(data[[old_stage_var]])
        if (verbose) message("Converted ", old_stage_var, " to factor")
    }
    if (!is.factor(data[[new_stage_var]])) {
        data[[new_stage_var]] <- as.factor(data[[new_stage_var]])
        if (verbose) message("Converted ", new_stage_var, " to factor")
    }

    # Get levels
    result$old_stage_levels <- levels(data[[old_stage_var]])
    result$new_stage_levels <- levels(data[[new_stage_var]])

    # Validate minimum levels
    if (length(result$old_stage_levels) < 2) {
        result$errors <- c(result$errors,
            paste("Old stage variable must have at least 2 levels, found:",
                  length(result$old_stage_levels)))
    }
    if (length(result$new_stage_levels) < 2) {
        result$errors <- c(result$errors,
            paste("New stage variable must have at least 2 levels, found:",
                  length(result$new_stage_levels)))
    }

    # Check for missing values
    old_missing <- sum(is.na(data[[old_stage_var]]))
    new_missing <- sum(is.na(data[[new_stage_var]]))

    if (old_missing > 0) {
        result$warnings <- c(result$warnings,
            paste(old_missing, "missing values in old stage variable"))
    }
    if (new_missing > 0) {
        result$warnings <- c(result$warnings,
            paste(new_missing, "missing values in new stage variable"))
    }

    # Check for special characters that might cause issues
    old_has_special <- any(grepl("[^A-Za-z0-9 ]", result$old_stage_levels))
    new_has_special <- any(grepl("[^A-Za-z0-9 ]", result$new_stage_levels))

    if (old_has_special) {
        result$warnings <- c(result$warnings,
            "Old stage levels contain special characters - will be handled safely")
    }
    if (new_has_special) {
        result$warnings <- c(result$warnings,
            "New stage levels contain special characters - will be handled safely")
    }

    result$data <- data
    return(result)
}

# =============================================================================
# SAFE EXECUTION UTILITIES
# =============================================================================

#' Safe Atomic Conversion
#'
#' @description
#' Safely convert values to atomic types with fallback defaults.
#'
#' @param value Value to convert
#' @param type Target type ("numeric", "integer", "character", "logical")
#' @param default Default value if conversion fails
#' @return Converted value or default
#' @keywords internal
stagemigration_safeAtomic <- function(value, type = "numeric", default = NA) {
    tryCatch({
        if (is.null(value) || length(value) == 0) {
            return(default)
        }

        # Convert based on type
        result <- switch(type,
            "numeric" = {
                val <- as.numeric(value)[1]
                if (is.finite(val)) val else default
            },
            "integer" = {
                val <- as.integer(value)[1]
                if (is.finite(val)) val else as.integer(default)
            },
            "character" = {
                val <- as.character(value)[1]
                if (is.na(val)) as.character(default) else val
            },
            "logical" = {
                val <- as.logical(value)[1]
                if (is.na(val)) as.logical(default) else val
            },
            default
        )

        return(result)
    }, error = function(e) {
        return(default)
    })
}

#' Safe Execution with Error Handling
#'
#' @description
#' Standardized error handling wrapper for consistent user experience.
#'
#' @param expr Expression to execute
#' @param errorReturn Value to return on error
#' @param errorMessage User-friendly error message
#' @param warningMessage Optional warning to show on error
#' @param silent If TRUE, suppress error messages
#' @param context Context string for debugging
#' @return Result of expression or errorReturn on failure
#' @keywords internal
stagemigration_safeExecute <- function(expr,
                                       errorReturn = NULL,
                                       errorMessage = "Operation failed",
                                       warningMessage = NULL,
                                       silent = FALSE,
                                       context = NULL) {
    result <- tryCatch({
        expr
    }, error = function(e) {
        if (!silent) {
            # Log detailed error for debugging
            context_str <- if (!is.null(context)) paste0("[", context, "] ") else ""
            message(paste0("DEBUG: ", context_str, errorMessage, " - ", e$message))

            # Show user-friendly warning if specified
            if (!is.null(warningMessage)) {
                warning(warningMessage, call. = FALSE)
            }
        }
        return(errorReturn)
    }, warning = function(w) {
        # Capture warnings but let execution continue
        if (!silent) {
            context_str <- if (!is.null(context)) paste0("[", context, "] ") else ""
            message(paste0("Warning in ", context_str, errorMessage, ": ", w$message))
        }
        # Re-evaluate the expression suppressing the warning
        suppressWarnings(expr)
    })

    return(result)
}

# =============================================================================
# FORMULA BUILDING UTILITIES
# =============================================================================

#' Build Safe Survival Formula
#'
#' @description
#' Constructs survival formula with properly escaped variable names.
#'
#' @param time_var Name of time variable
#' @param event_var Name of event variable
#' @param predictors Character vector of predictor variable names
#' @param interaction Logical, whether to include interactions
#' @return Formula object
#' @keywords internal
stagemigration_buildFormula <- function(time_var, event_var, predictors, interaction = FALSE) {
    # Escape variable names
    time_safe <- stagemigration_escapeVar(time_var, backticks = TRUE)
    event_safe <- stagemigration_escapeVar(event_var, backticks = TRUE)

    # Build LHS (left-hand side)
    lhs <- paste0("survival::Surv(", time_safe, ", ", event_safe, ")")

    # Build RHS (right-hand side)
    if (length(predictors) == 0) {
        return(as.formula(paste(lhs, "~ 1")))
    }

    # Escape predictor names
    pred_safe <- sapply(predictors, function(p) {
        stagemigration_escapeVar(p, backticks = TRUE)
    })

    if (interaction && length(pred_safe) > 1) {
        # Include interactions
        rhs <- paste(pred_safe, collapse = " * ")
    } else {
        # Additive model
        rhs <- paste(pred_safe, collapse = " + ")
    }

    formula_str <- paste(lhs, "~", rhs)
    return(as.formula(formula_str))
}

# =============================================================================
# DATA QUALITY UTILITIES
# =============================================================================

#' Check Sample Size Adequacy
#'
#' @description
#' Validates sample size and event rates for meaningful analysis.
#'
#' @param n Total sample size
#' @param n_events Number of events
#' @param n_predictors Number of predictor variables
#' @param analysis_type Type of analysis ("basic", "standard", "comprehensive")
#' @return List with adequacy assessment and recommendations
#' @keywords internal
stagemigration_checkSampleSize <- function(n, n_events, n_predictors = 2,
                                           analysis_type = "standard") {
    result <- list(
        adequate = FALSE,
        level = "INSUFFICIENT",
        event_rate = n_events / n,
        events_per_variable = n_events / n_predictors,
        messages = character(0),
        recommendations = character(0)
    )

    result$event_rate <- n_events / n
    result$events_per_variable <- n_events / n_predictors

    # Minimum thresholds based on analysis type
    min_events <- switch(analysis_type,
        "basic" = 20,
        "standard" = 50,
        "comprehensive" = 100,
        50  # default
    )

    # Events per variable rule (Harrell: 10-20 EPV recommended)
    min_epv <- 10

    # Check total events
    if (n_events < 10) {
        result$level <- "CRITICAL"
        result$messages <- c(result$messages,
            paste("CRITICAL: Only", n_events, "events - results unreliable"))
        result$recommendations <- c(result$recommendations,
            "Collect more data or consider descriptive analysis only")
    } else if (n_events < 20) {
        result$level <- "POOR"
        result$messages <- c(result$messages,
            paste("WARNING:", n_events, "events - minimal for basic analysis"))
        result$recommendations <- c(result$recommendations,
            "Results should be interpreted with caution",
            "Avoid complex models and bootstrap validation")
    } else if (n_events < min_events) {
        result$level <- "MARGINAL"
        result$messages <- c(result$messages,
            paste("NOTICE:", n_events, "events - adequate for", analysis_type, "analysis"))
        result$adequate <- TRUE
    } else {
        result$level <- "ADEQUATE"
        result$adequate <- TRUE
        result$messages <- c(result$messages,
            paste("Sample size adequate:", n_events, "events"))
    }

    # Check events per variable
    if (result$events_per_variable < min_epv) {
        result$messages <- c(result$messages,
            sprintf("Events per variable (%.1f) below recommended minimum (%d)",
                    result$events_per_variable, min_epv))
        result$recommendations <- c(result$recommendations,
            "Consider fewer predictors or collect more data")
    }

    # Check event rate
    if (result$event_rate < 0.05 || result$event_rate > 0.95) {
        result$messages <- c(result$messages,
            sprintf("Event rate (%.1f%%) very unbalanced", result$event_rate * 100))
        result$recommendations <- c(result$recommendations,
            "Consider alternative methods for rare events")
    }

    return(result)
}

#' Generate Data Quality Report
#'
#' @description
#' Creates comprehensive data quality assessment for stage migration analysis.
#'
#' @param data Data frame
#' @param old_stage Old staging variable name
#' @param new_stage New staging variable name
#' @param time_var Survival time variable name
#' @param event_var Event indicator variable name
#' @return List with quality metrics and recommendations
#' @keywords internal
stagemigration_dataQualityReport <- function(data, old_stage, new_stage,
                                             time_var, event_var) {
    report <- list(
        overall_quality = "UNKNOWN",
        n_total = nrow(data),
        n_complete = 0,
        missing_summary = list(),
        stage_summary = list(),
        survival_summary = list(),
        warnings = character(0),
        recommendations = character(0)
    )

    # Missing data analysis
    vars_to_check <- c(old_stage, new_stage, time_var, event_var)
    for (var in vars_to_check) {
        if (!var %in% names(data)) {
            report$warnings <- c(report$warnings,
                paste("Variable not found:", var))
            next
        }

        n_missing <- sum(is.na(data[[var]]))
        pct_missing <- (n_missing / nrow(data)) * 100

        report$missing_summary[[var]] <- list(
            n_missing = n_missing,
            pct_missing = pct_missing
        )

        if (pct_missing > 20) {
            report$warnings <- c(report$warnings,
                sprintf("%s has %.1f%% missing values", var, pct_missing))
        }
    }

    # Complete cases
    complete_vars <- intersect(vars_to_check, names(data))
    report$n_complete <- sum(complete.cases(data[, complete_vars, drop = FALSE]))

    # Stage distribution analysis
    if (old_stage %in% names(data) && new_stage %in% names(data)) {
        report$stage_summary$old_stages <- table(data[[old_stage]], useNA = "ifany")
        report$stage_summary$new_stages <- table(data[[new_stage]], useNA = "ifany")

        # Check for single-case stages (problematic for analysis)
        old_singles <- sum(report$stage_summary$old_stages == 1)
        new_singles <- sum(report$stage_summary$new_stages == 1)

        if (old_singles > 0) {
            report$warnings <- c(report$warnings,
                paste(old_singles, "old stage levels with only 1 patient"))
        }
        if (new_singles > 0) {
            report$warnings <- c(report$warnings,
                paste(new_singles, "new stage levels with only 1 patient"))
        }
    }

    # Survival data analysis
    if (time_var %in% names(data) && event_var %in% names(data)) {
        time_data <- data[[time_var]][!is.na(data[[time_var]])]
        event_data <- data[[event_var]][!is.na(data[[event_var]])]

        report$survival_summary <- list(
            median_followup = median(time_data, na.rm = TRUE),
            max_followup = max(time_data, na.rm = TRUE),
            n_events = sum(event_data == 1, na.rm = TRUE),
            event_rate = mean(event_data == 1, na.rm = TRUE)
        )

        # Check for zero follow-up times
        n_zero_time <- sum(time_data == 0, na.rm = TRUE)
        if (n_zero_time > 0) {
            report$warnings <- c(report$warnings,
                paste(n_zero_time, "patients with zero follow-up time"))
        }
    }

    # Overall quality assessment
    n_warnings <- length(report$warnings)
    if (n_warnings == 0 && report$n_complete >= 30) {
        report$overall_quality <- "GOOD"
    } else if (n_warnings <= 2 && report$n_complete >= 20) {
        report$overall_quality <- "ACCEPTABLE"
    } else if (report$n_complete >= 10) {
        report$overall_quality <- "POOR"
        report$recommendations <- c(report$recommendations,
            "Data quality issues detected - review data carefully")
    } else {
        report$overall_quality <- "INADEQUATE"
        report$recommendations <- c(report$recommendations,
            "Insufficient complete cases for meaningful analysis")
    }

    return(report)
}

# =============================================================================
# CONSTANTS AND THRESHOLDS
# =============================================================================

#' Stage Migration Analysis Constants
#'
#' @description
#' Centralized constants used throughout stage migration analysis to avoid
#' magic numbers and ensure consistency.
#'
#' @keywords internal
STAGEMIGRATION_CONSTANTS <- list(
    # Sample size thresholds
    MIN_EVENTS_CRITICAL = 10,
    MIN_EVENTS_WARNING = 20,
    MIN_EVENTS_ADEQUATE = 50,
    MIN_EVENTS_OPTIMAL = 100,

    # Events per variable (Harrell's rule)
    MIN_EPV = 10,
    RECOMMENDED_EPV = 15,

    # Event rate bounds
    MIN_EVENT_RATE = 0.05,
    MAX_EVENT_RATE = 0.95,

    # Bootstrap defaults
    BOOTSTRAP_DEFAULT_REPS = 1000,
    BOOTSTRAP_MIN_REPS = 200,
    BOOTSTRAP_MAX_REPS = 5000,

    # Statistical thresholds
    ALPHA = 0.05,
    CI_LEVEL = 0.95,

    # C-index interpretation (Harrell)
    CINDEX_POOR = 0.60,
    CINDEX_FAIR = 0.70,
    CINDEX_GOOD = 0.80,
    CINDEX_EXCELLENT = 0.90,

    # Effect size thresholds (for C-index difference)
    EFFECT_TRIVIAL = 0.01,
    EFFECT_SMALL = 0.02,
    EFFECT_MEDIUM = 0.05,
    EFFECT_LARGE = 0.10,

    # Will Rogers phenomenon thresholds
    WILL_ROGERS_MART_THRESHOLD = 2.5,  # Standard deviations
    WILL_ROGERS_MIN_MIGRATION_RATE = 0.10,  # 10%

    # Missing data tolerance
    MAX_MISSING_PCT = 20,

    # Outlier detection
    OUTLIER_SD_THRESHOLD = 3.0
)
