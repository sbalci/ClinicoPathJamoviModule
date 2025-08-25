#' ClinicoPath Enhanced Error Handling Framework
#' 
#' Robust error handling system inspired by BlueSky R environment
#' with enhanced features for clinical and pathological research.
#' 
#' @import jmvcore
#' @export

# Global error and warning tracking
.clinicopath_errors <- new.env(parent = emptyenv())
.clinicopath_errors$error_count <- 0
.clinicopath_errors$warning_count <- 0
.clinicopath_errors$error_log <- list()
.clinicopath_errors$warning_log <- list()
.clinicopath_errors$function_stack <- c()

#' Initialize ClinicoPath Error Handling System
#' 
#' @description Initialize error handling for a ClinicoPath function
#' @param function_name Name of the function being initialized
#' @param context Additional context information
#' @export
clinicopath_init <- function(function_name, context = "") {
    # Reset counters for new function call
    .clinicopath_errors$error_count <- 0
    .clinicopath_errors$warning_count <- 0
    .clinicopath_errors$error_log <- list()
    .clinicopath_errors$warning_log <- list()
    
    # Track function stack
    .clinicopath_errors$function_stack <- c(.clinicopath_errors$function_stack, function_name)
    
    # Log function initialization
    timestamp <- Sys.time()
    init_log <- list(
        timestamp = timestamp,
        function_name = function_name,
        context = context,
        call_stack = sys.calls()
    )
    
    .clinicopath_errors$init_log <- init_log
    
    invisible(TRUE)
}

#' ClinicoPath Error Handler
#' 
#' @description Enhanced error handler with clinical context
#' @param error The error object
#' @param function_name Name of the function where error occurred
#' @param clinical_context Clinical context for the error
#' @export
clinicopath_error_handler <- function(error, function_name = "unknown", clinical_context = "") {
    .clinicopath_errors$error_count <- .clinicopath_errors$error_count + 1
    
    # Create detailed error log
    error_entry <- list(
        timestamp = Sys.time(),
        function_name = function_name,
        error_message = conditionMessage(error),
        error_class = class(error),
        clinical_context = clinical_context,
        call_stack = sys.calls(),
        function_stack = .clinicopath_errors$function_stack,
        error_number = .clinicopath_errors$error_count,
        session_info = sessionInfo()$R.version$version.string
    )
    
    .clinicopath_errors$error_log[[.clinicopath_errors$error_count]] <- error_entry
    
    # Generate user-friendly error message
    user_message <- generate_user_friendly_error(error_entry)
    
    # Log to console with clinical context
    cat("\n=== ClinicoPath Error Handler ===\n")
    cat("Function:", function_name, "\n")
    cat("Error:", conditionMessage(error), "\n")
    if (clinical_context != "") {
        cat("Clinical Context:", clinical_context, "\n")
    }
    cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Error ID:", .clinicopath_errors$error_count, "\n")
    cat("=== End Error Report ===\n\n")
    
    return(list(
        success = FALSE,
        error = TRUE,
        error_message = user_message,
        error_id = .clinicopath_errors$error_count,
        detailed_log = error_entry
    ))
}

#' ClinicoPath Warning Handler
#' 
#' @description Enhanced warning handler with clinical context
#' @param warning The warning object
#' @param function_name Name of the function where warning occurred
#' @param clinical_context Clinical context for the warning
#' @export
clinicopath_warning_handler <- function(warning, function_name = "unknown", clinical_context = "") {
    .clinicopath_errors$warning_count <- .clinicopath_errors$warning_count + 1
    
    # Create detailed warning log
    warning_entry <- list(
        timestamp = Sys.time(),
        function_name = function_name,
        warning_message = conditionMessage(warning),
        warning_class = class(warning),
        clinical_context = clinical_context,
        call_stack = sys.calls(),
        function_stack = .clinicopath_errors$function_stack,
        warning_number = .clinicopath_errors$warning_count
    )
    
    .clinicopath_errors$warning_log[[.clinicopath_errors$warning_count]] <- warning_entry
    
    # Generate user-friendly warning message
    user_message <- generate_user_friendly_warning(warning_entry)
    
    # Log to console with clinical context
    cat("\n=== ClinicoPath Warning ===\n")
    cat("Function:", function_name, "\n")
    cat("Warning:", conditionMessage(warning), "\n")
    if (clinical_context != "") {
        cat("Clinical Context:", clinical_context, "\n")
    }
    cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    cat("=== End Warning Report ===\n\n")
    
    return(list(
        success = TRUE,
        warning = TRUE,
        warning_message = user_message,
        warning_id = .clinicopath_errors$warning_count,
        detailed_log = warning_entry
    ))
}

#' Safe Execution Wrapper
#' 
#' @description Safely execute code with enhanced error handling
#' @param expr Expression to execute
#' @param function_name Name of the calling function
#' @param clinical_context Clinical context for the operation
#' @param default_value Default value to return on error
#' @export
safe_execute <- function(expr, function_name = "unknown", clinical_context = "", default_value = NULL) {
    
    result <- tryCatch(
        withCallingHandlers(
            {
                eval(expr)
            },
            warning = function(w) {
                warning_result <- clinicopath_warning_handler(w, function_name, clinical_context)
                invokeRestart("muffleWarning")
            }
        ),
        error = function(e) {
            error_result <- clinicopath_error_handler(e, function_name, clinical_context)
            return(list(
                success = FALSE,
                result = default_value,
                error_info = error_result
            ))
        }
    )
    
    return(list(
        success = TRUE,
        result = result,
        warnings = .clinicopath_errors$warning_count,
        errors = .clinicopath_errors$error_count
    ))
}

#' Validate Data with Clinical Context
#' 
#' @description Validate data with clinical research considerations
#' @param data The data to validate
#' @param required_vars Required variable names
#' @param min_observations Minimum number of observations required
#' @param clinical_checks Additional clinical validation checks
#' @export
validate_clinical_data <- function(data, required_vars = NULL, min_observations = 1, clinical_checks = TRUE) {
    validation_errors <- c()
    validation_warnings <- c()
    
    # Basic data validation
    if (is.null(data)) {
        validation_errors <- c(validation_errors, "Data is NULL")
    } else if (nrow(data) == 0) {
        validation_errors <- c(validation_errors, "Data contains no observations")
    } else if (ncol(data) == 0) {
        validation_errors <- c(validation_errors, "Data contains no variables")
    }
    
    # Check minimum observations
    if (!is.null(data) && nrow(data) < min_observations) {
        validation_errors <- c(validation_errors, 
                              paste("Insufficient observations: found", nrow(data), "but need at least", min_observations))
    }
    
    # Check required variables
    if (!is.null(required_vars) && !is.null(data)) {
        missing_vars <- required_vars[!required_vars %in% names(data)]
        if (length(missing_vars) > 0) {
            validation_errors <- c(validation_errors, 
                                  paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
        }
    }
    
    # Clinical data quality checks
    if (clinical_checks && !is.null(data) && nrow(data) > 0) {
        
        # Check for excessive missing data
        missing_proportions <- sapply(data, function(x) sum(is.na(x)) / length(x))
        high_missing <- missing_proportions > 0.5
        if (any(high_missing)) {
            validation_warnings <- c(validation_warnings,
                                   paste("Variables with >50% missing data:", 
                                        paste(names(data)[high_missing], collapse = ", ")))
        }
        
        # Check for constant variables
        constant_vars <- sapply(data, function(x) {
            if (is.numeric(x)) {
                var(x, na.rm = TRUE) == 0 || is.na(var(x, na.rm = TRUE))
            } else {
                length(unique(x[!is.na(x)])) <= 1
            }
        })
        
        if (any(constant_vars)) {
            validation_warnings <- c(validation_warnings,
                                   paste("Constant variables detected:", 
                                        paste(names(data)[constant_vars], collapse = ", ")))
        }
        
        # Check for extreme outliers in numeric variables
        numeric_vars <- sapply(data, is.numeric)
        if (any(numeric_vars)) {
            outlier_vars <- c()
            for (var in names(data)[numeric_vars]) {
                values <- data[[var]][!is.na(data[[var]])]
                if (length(values) > 10) {
                    q1 <- quantile(values, 0.25)
                    q3 <- quantile(values, 0.75)
                    iqr <- q3 - q1
                    outliers <- sum(values < (q1 - 3 * iqr) | values > (q3 + 3 * iqr))
                    if (outliers > length(values) * 0.05) {  # More than 5% outliers
                        outlier_vars <- c(outlier_vars, var)
                    }
                }
            }
            if (length(outlier_vars) > 0) {
                validation_warnings <- c(validation_warnings,
                                       paste("Variables with potential outliers:", 
                                            paste(outlier_vars, collapse = ", ")))
            }
        }
        
        # Check sample size recommendations for clinical research
        if (nrow(data) < 30) {
            validation_warnings <- c(validation_warnings,
                                   "Small sample size (n < 30): Results may have limited statistical power")
        }
    }
    
    return(list(
        valid = length(validation_errors) == 0,
        errors = validation_errors,
        warnings = validation_warnings,
        n_observations = ifelse(is.null(data), 0, nrow(data)),
        n_variables = ifelse(is.null(data), 0, ncol(data))
    ))
}

#' Generate User-Friendly Error Message
#' 
#' @description Convert technical error to user-friendly clinical message
#' @param error_entry Detailed error log entry
#' @export
generate_user_friendly_error <- function(error_entry) {
    error_msg <- error_entry$error_message
    function_name <- error_entry$function_name
    
    # Common error patterns and user-friendly translations
    if (grepl("object.*not found", error_msg, ignore.case = TRUE)) {
        return(paste("Required data or variable not found in", function_name, 
                    "analysis. Please check your variable selections."))
    } else if (grepl("subscript out of bounds", error_msg, ignore.case = TRUE)) {
        return(paste("Data access error in", function_name, 
                    ". This may indicate insufficient data or incorrect variable specification."))
    } else if (grepl("non-numeric argument", error_msg, ignore.case = TRUE)) {
        return(paste("Non-numeric data provided to", function_name, 
                    "where numeric data is required. Please check variable types."))
    } else if (grepl("infinite.*produced", error_msg, ignore.case = TRUE)) {
        return(paste("Mathematical overflow in", function_name, 
                    ". This may indicate extreme values or division by zero."))
    } else if (grepl("singular", error_msg, ignore.case = TRUE)) {
        return(paste("Matrix singularity detected in", function_name, 
                    ". This often indicates multicollinearity or insufficient variation in data."))
    } else if (grepl("convergence", error_msg, ignore.case = TRUE)) {
        return(paste("Analysis in", function_name, "failed to converge.", 
                    "Try adjusting parameters or check data quality."))
    } else {
        return(paste("An error occurred in", function_name, "analysis:", error_msg,
                    "Please check your data and analysis parameters."))
    }
}

#' Generate User-Friendly Warning Message
#' 
#' @description Convert technical warning to user-friendly clinical message
#' @param warning_entry Detailed warning log entry
#' @export
generate_user_friendly_warning <- function(warning_entry) {
    warning_msg <- warning_entry$warning_message
    function_name <- warning_entry$function_name
    
    # Common warning patterns and user-friendly translations
    if (grepl("NAs introduced", warning_msg, ignore.case = TRUE)) {
        return(paste("Some data converted to missing values in", function_name, 
                    ". This may affect analysis results."))
    } else if (grepl("singular fit", warning_msg, ignore.case = TRUE)) {
        return(paste("Model fitting issue in", function_name, 
                    ". Results may be unreliable due to data characteristics."))
    } else if (grepl("iteration limit", warning_msg, ignore.case = TRUE)) {
        return(paste("Iteration limit reached in", function_name, 
                    ". Consider increasing iteration limits or checking convergence."))
    } else {
        return(paste("Warning in", function_name, "analysis:", warning_msg))
    }
}

#' Get Error Summary
#' 
#' @description Get summary of errors and warnings
#' @export
get_error_summary <- function() {
    return(list(
        error_count = .clinicopath_errors$error_count,
        warning_count = .clinicopath_errors$warning_count,
        last_function = tail(.clinicopath_errors$function_stack, 1),
        has_errors = .clinicopath_errors$error_count > 0,
        has_warnings = .clinicopath_errors$warning_count > 0
    ))
}

#' Clear Error Log
#' 
#' @description Clear the error and warning logs
#' @export
clear_error_log <- function() {
    .clinicopath_errors$error_count <- 0
    .clinicopath_errors$warning_count <- 0
    .clinicopath_errors$error_log <- list()
    .clinicopath_errors$warning_log <- list()
    .clinicopath_errors$function_stack <- c()
    invisible(TRUE)
}

#' Function Cleanup
#' 
#' @description Clean up function execution context
#' @param function_name Name of the function being cleaned up
#' @export
clinicopath_cleanup <- function(function_name) {
    # Remove function from stack
    if (length(.clinicopath_errors$function_stack) > 0) {
        stack_length <- length(.clinicopath_errors$function_stack)
        if (.clinicopath_errors$function_stack[stack_length] == function_name) {
            .clinicopath_errors$function_stack <- .clinicopath_errors$function_stack[-stack_length]
        }
    }
    
    # Log cleanup
    cat("ClinicoPath cleanup completed for function:", function_name, "\n")
    
    invisible(TRUE)
}

#' Clinical Context Wrapper
#' 
#' @description Wrapper for clinical research contexts
#' @param context_type Type of clinical context (survival, diagnostic, screening, etc.)
#' @param sample_size Sample size for power considerations
#' @param effect_size Expected effect size
#' @export
get_clinical_context <- function(context_type = "general", sample_size = NULL, effect_size = NULL) {
    context_info <- list(
        type = context_type,
        sample_size = sample_size,
        effect_size = effect_size
    )
    
    # Add context-specific recommendations
    if (context_type == "survival") {
        context_info$recommendations <- c(
            "Ensure adequate follow-up time",
            "Check proportional hazards assumption",
            "Consider competing risks if applicable"
        )
    } else if (context_type == "diagnostic") {
        context_info$recommendations <- c(
            "Verify reference standard quality",
            "Check spectrum bias",
            "Consider prevalence effects on predictive values"
        )
    } else if (context_type == "screening") {
        context_info$recommendations <- c(
            "Consider overdiagnosis potential",
            "Evaluate lead time bias",
            "Assess interval cancer rates"
        )
    }
    
    return(context_info)
}

#' Enhanced Result Structure
#' 
#' @description Create enhanced result structure with error handling metadata
#' @param results Main analysis results
#' @param function_name Name of the analysis function
#' @param success Whether the analysis succeeded
#' @export
create_enhanced_result <- function(results, function_name, success = TRUE) {
    enhanced_result <- list(
        results = results,
        metadata = list(
            function_name = function_name,
            timestamp = Sys.time(),
            success = success,
            error_count = .clinicopath_errors$error_count,
            warning_count = .clinicopath_errors$warning_count,
            r_version = R.version.string
        )
    )
    
    if (.clinicopath_errors$error_count > 0) {
        enhanced_result$errors <- .clinicopath_errors$error_log
    }
    
    if (.clinicopath_errors$warning_count > 0) {
        enhanced_result$warnings <- .clinicopath_errors$warning_log
    }
    
    class(enhanced_result) <- c("clinicopath_result", class(enhanced_result))
    return(enhanced_result)
}