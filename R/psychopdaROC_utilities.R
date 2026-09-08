# ============================================================================
# UTILITY FUNCTIONS FOR PSYCHOPDAROC
# ============================================================================
# This file contains shared utility functions used by the psychopdaROC module
# Functions here should be generic and reusable, not class-specific
# 
# Note: Core utility functions have been moved to R/utils.R
# This file now contains psychopdaROC-specific utility functions only

#' Bootstrap NRI calculation with confidence intervals
#'
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector (0/1)
#' @param direction Classification direction (">=" or "<=")
#' @param thresholds Risk category thresholds (NULL for continuous NRI)
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence level (default 0.95)
#' @return List with NRI components and confidence intervals
#' @export
bootstrapNRI <- function(new_values, ref_values, actual,
                         direction = ">=", thresholds = NULL,
                         n_boot = 1000, conf_level = 0.95) {
    # Validate inputs
    n <- length(actual)
    if (length(new_values) != n || length(ref_values) != n) {
        stop("All input vectors must have the same length")
    }

    if (!all(actual %in% c(0, 1))) {
        stop("Actual values must be binary (0 or 1)")
    }

    # Validate thresholds if provided
    if (!is.null(thresholds)) {
        thresholds <- sort(thresholds)
        if (any(thresholds <= 0) || any(thresholds >= 1)) {
            stop("Thresholds must be between 0 and 1")
        }
    }

    # Original NRI calculation
    original_nri <- computeNRI(
        new_values, ref_values, actual, direction, thresholds, warn = FALSE
    )

    # Bootstrap
    boot_nri <- numeric(n_boot)
    boot_event_nri <- numeric(n_boot)
    boot_non_event_nri <- numeric(n_boot)
    valid_boots <- 0
    warning_boots <- 0L

    for (i in 1:n_boot) {
        boot_idx <- sample(n, n, replace = TRUE)

        boot_new <- new_values[boot_idx]
        boot_ref <- ref_values[boot_idx]
        boot_actual <- actual[boot_idx]

        # Skip if no events or non-events
        if (sum(boot_actual == 1) == 0 || sum(boot_actual == 0) == 0) {
            boot_nri[i] <- NA
            boot_event_nri[i] <- NA
            boot_non_event_nri[i] <- NA
            next
        }

        # Take the result FROM tryCatch, not from inside the error handler.
        #
        # `boot_nri[i] <- NA` written in the handler assigns into that function's
        # own frame and never reaches this one, so a failed replicate kept its
        # preallocated 0.0 -- which then survived the !is.na() filter below and
        # was averaged into the confidence interval and counted in both p-value
        # tails as though it were a real estimate of no reclassification. The
        # "many bootstraps failed" warning could never fire from this path
        # either. Same defect as bootstrapIDI() in utils.R.
        res_i <- tryCatch({
            r <- computeNRI(
                boot_new, boot_ref, boot_actual, direction, thresholds, warn = FALSE
            )
            if (isTRUE(r$fit_warning)) warning_boots <- warning_boots + 1L
            if (is.null(r$nri) || is.na(r$nri)) NULL else r
        }, error = function(e) NULL)

        if (is.null(res_i)) {
            boot_nri[i] <- NA
            boot_event_nri[i] <- NA
            boot_non_event_nri[i] <- NA
        } else {
            boot_nri[i] <- res_i$nri
            boot_event_nri[i] <- res_i$event_nri
            boot_non_event_nri[i] <- res_i$non_event_nri
            valid_boots <- valid_boots + 1
        }
    }

    # Remove failed iterations
    valid_idx <- !is.na(boot_nri)
    boot_nri <- boot_nri[valid_idx]
    boot_event_nri <- boot_event_nri[valid_idx]
    boot_non_event_nri <- boot_non_event_nri[valid_idx]

    if (length(boot_nri) == 0L) {
        warning("All bootstrap replicates failed; no confidence interval or p-value can be computed.",
                call. = FALSE)
        return(list(
            nri = original_nri$nri,
            event_nri = original_nri$event_nri,
            non_event_nri = original_nri$non_event_nri,
            ci_lower = NA_real_, ci_upper = NA_real_, p_value = NA_real_,
            n_valid_boots = 0L,
            fit_warning = isTRUE(original_nri$fit_warning),
            fit_warning_boots = warning_boots
        ))
    }

    if (length(boot_nri) < n_boot * 0.5) {
        warning("Many bootstrap samples failed - results may be unreliable")
    }
    if (isTRUE(original_nri$fit_warning) || warning_boots > 0L) {
        warning(sprintf(
            paste0(
                "Logistic calibration showed separation or non-convergence in the original ",
                "fit or %d of %d bootstrap replicates; NRI and its interval may be unstable."
            ),
            warning_boots, n_boot
        ), call. = FALSE)
    }

    # Calculate confidence intervals
    alpha <- 1 - conf_level
    ci_lower <- quantile(boot_nri, alpha/2, na.rm = TRUE)
    ci_upper <- quantile(boot_nri, 1 - alpha/2, na.rm = TRUE)

    # Calculate p-value.
    #
    # (1 + count) / (B + 1) rather than count / B -- the uncorrected form
    # returns exactly 0 whenever every replicate falls on one side, implying
    # precision a finite bootstrap cannot deliver. Same correction as
    # bootstrapIDI() in utils.R.
    boot_nri_valid <- boot_nri[!is.na(boot_nri)]
    B <- length(boot_nri_valid)
    p_value <- if (B == 0) NA_real_ else min(1, 2 * min(
        (1 + sum(boot_nri_valid <= 0)) / (B + 1),
        (1 + sum(boot_nri_valid >= 0)) / (B + 1)
    ))

    return(list(
        nri = original_nri$nri,
        event_nri = original_nri$event_nri,
        non_event_nri = original_nri$non_event_nri,
        ci_lower = as.numeric(ci_lower),
        ci_upper = as.numeric(ci_upper),
        p_value = p_value,
        n_valid_boots = valid_boots,
        fit_warning = isTRUE(original_nri$fit_warning),
        fit_warning_boots = warning_boots
    ))
}

#' Compute Net Reclassification Index (NRI)
#'
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector (0/1)
#' @param direction Classification direction
#' @param thresholds Risk category thresholds (NULL for continuous NRI)
#' @param warn Emit a warning when the logistic calibration shows separation or
#'   fails to converge. Set to `FALSE` inside bootstrap replicates.
#' @return List containing NRI components
computeNRI <- function(new_values, ref_values, actual,
                       direction = ">=", thresholds = NULL, warn = TRUE) {
    # Convert to probabilities
    new_probs <- raw_to_prob(new_values, actual, direction, warn = FALSE)
    ref_probs <- raw_to_prob(ref_values, actual, direction, warn = FALSE)
    fit_warning <- length(attr(new_probs, "fit_warnings")) > 0L ||
        length(attr(ref_probs, "fit_warnings")) > 0L
    if (isTRUE(warn) && fit_warning) {
        warning(
            paste0(
                "Logistic calibration showed separation or non-convergence; ",
                "NRI may be unstable."
            ),
            call. = FALSE
        )
    }

    # Identify events and non-events
    events <- actual == 1
    non_events <- actual == 0

    # Check for sufficient data
    if (sum(events) == 0 || sum(non_events) == 0) {
        return(list(nri = NA, event_nri = NA, non_event_nri = NA,
                    fit_warning = fit_warning))
    }

    # Unusable predicted probabilities must yield NA, not 0.
    #
    # Every count below is taken with na.rm = TRUE, so if raw_to_prob could not
    # fit a model and returned NA, all four counts come out 0, every proportion
    # comes out 0, and the NRI is reported as exactly 0 -- which reads as "the
    # new marker reclassifies nobody", a substantive negative finding
    # manufactured out of a fitting failure.
    if (all(is.na(new_probs)) || all(is.na(ref_probs))) {
        if (isTRUE(warn)) {
            warning("computeNRI: predicted probabilities are unavailable (the logistic fit failed); returning NA rather than an NRI of 0.",
                    call. = FALSE)
        }
        return(list(nri = NA, event_nri = NA, non_event_nri = NA,
                    fit_warning = TRUE))
    }

    if (is.null(thresholds) || length(thresholds) == 0) {
        # Continuous NRI
        up_events <- sum(new_probs[events] > ref_probs[events], na.rm = TRUE)
        down_events <- sum(new_probs[events] < ref_probs[events], na.rm = TRUE)
        up_non_events <- sum(new_probs[non_events] > ref_probs[non_events], na.rm = TRUE)
        down_non_events <- sum(new_probs[non_events] < ref_probs[non_events], na.rm = TRUE)

        # Calculate proportions
        p_up_events <- up_events / sum(events)
        p_down_events <- down_events / sum(events)
        p_up_non_events <- up_non_events / sum(non_events)
        p_down_non_events <- down_non_events / sum(non_events)

    } else {
        # Categorical NRI
        # Create risk categories with proper handling of boundaries
        breaks <- c(0, thresholds, 1)
        labels <- 1:(length(breaks) - 1)

        ref_cats <- cut(ref_probs,
                        breaks = breaks,
                        labels = labels,
                        include.lowest = TRUE)

        new_cats <- cut(new_probs,
                        breaks = breaks,
                        labels = labels,
                        include.lowest = TRUE)

        # Calculate movement between categories
        move_up_event <- sum(as.numeric(new_cats[events]) > as.numeric(ref_cats[events]), na.rm = TRUE)
        move_down_event <- sum(as.numeric(new_cats[events]) < as.numeric(ref_cats[events]), na.rm = TRUE)
        move_up_non_event <- sum(as.numeric(new_cats[non_events]) > as.numeric(ref_cats[non_events]), na.rm = TRUE)
        move_down_non_event <- sum(as.numeric(new_cats[non_events]) < as.numeric(ref_cats[non_events]), na.rm = TRUE)

        # Calculate proportions
        p_up_events <- move_up_event / sum(events)
        p_down_events <- move_down_event / sum(events)
        p_up_non_events <- move_up_non_event / sum(non_events)
        p_down_non_events <- move_down_non_event / sum(non_events)
    }

    # Calculate NRI components
    event_nri <- p_up_events - p_down_events
    non_event_nri <- p_down_non_events - p_up_non_events
    overall_nri <- event_nri + non_event_nri

    return(list(
        nri = overall_nri,
        event_nri = event_nri,
        non_event_nri = non_event_nri,
        p_up_events = p_up_events,
        p_down_events = p_down_events,
        p_up_non_events = p_up_non_events,
        p_down_non_events = p_down_non_events,
        fit_warning = fit_warning
    ))
}

# validateROCInputs function moved to R/utils.R

# ============================================================================

# ROC ANALYSIS UTILITIES (moved from R/utils.R, 2026-09-06 audit)

# ============================================================================

# Only psychopdaROC uses these, so they ship with meddecide/jsurvival/JamoviTest.

#' Convert raw test values to predicted probabilities using ROC curve
#' @description Maps raw test values to probabilities based on their position in the ROC curve
#' @param values Raw test values
#' @param actual Binary outcomes (0/1)
#' @param direction Direction of test (">=" or "<=")
#' @keywords internal
#' @return Vector of predicted probabilities
#' @param warn Logical; emit a consolidated warning when logistic calibration
#'   shows separation, non-convergence, or fitting failure. Bootstrap callers
#'   set this to `FALSE` and report one aggregate diagnostic instead.
#' @export
raw_to_prob <- function(values, actual, direction = ">=", warn = TRUE) {
    # Validate inputs
    if (length(values) != length(actual)) {
        stop("Values and actual must have the same length")
    }

    if (!direction %in% c(">=", "<=")) {
        stop("Direction must be '>=' or '<='")
    }

    # Remove missing values
    complete_cases <- !is.na(values) & !is.na(actual)
    values_clean <- values[complete_cases]
    actual_clean <- actual[complete_cases]

    if (length(values_clean) == 0) {
        warning("No complete cases found")
        return(rep(NA, length(values)))
    }

    # Initialize probabilities vector
    probs <- rep(NA, length(values))

    # Use logistic regression to convert raw values to predicted probabilities
    # This is the statistically correct approach for IDI/NRI calculations
    predictor <- if (direction == "<=") -values_clean else values_clean

    fit_warnings <- character(0)
    glm_result <- tryCatch(
        withCallingHandlers({
            model <- stats::glm(actual_clean ~ predictor, family = binomial(link = "logit"))
            new_predictor <- if (direction == "<=") -values else values
            stats::predict(model, newdata = data.frame(predictor = new_predictor), type = "response")
        }, warning = function(w) {
            fit_warnings <<- c(fit_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }),
        error = function(e) {
        # Fail rather than substitute a non-probability.
        #
        # The previous fallback returned rank(x)/(n+1) -- the empirical
        # percentile of the PREDICTOR. It never touched `actual`, so its mean is
        # ~0.5 whatever the outcome prevalence and it is not an estimate of
        # P(Y = 1 | x) in any sense. Feeding that to IDI (a difference of mean
        # predicted risk between events and non-events) or to NRI category cuts
        # silently swaps a calibrated risk scale for a uniform rank scale.
        fit_warnings <<- c(fit_warnings, paste("fit failed:", conditionMessage(e)))
        rep(NA_real_, length(values))
    })

    probs <- as.numeric(glm_result)
    fit_warnings <- unique(fit_warnings)
    attr(probs, "fit_warnings") <- fit_warnings

    if (isTRUE(warn) && length(fit_warnings) > 0L) {
        warning(sprintf(
            paste0(
                "raw_to_prob: logistic calibration reported %s. Predicted probabilities ",
                "may be unreliable (separation or non-convergence); interpret IDI/NRI ",
                "with caution."
            ),
            paste(sprintf("'%s'", fit_warnings), collapse = "; ")
        ), call. = FALSE)
    }

    # Ensure probabilities are in [0,1] range
    probs[!is.na(probs) & probs < 0] <- 0
    probs[!is.na(probs) & probs > 1] <- 1

    return(probs)
}

#' Bootstrap IDI calculation with confidence intervals
#' @description Calculates Integrated Discrimination Improvement with bootstrap confidence intervals
#' @param new_values Test values for new test
#' @param ref_values Test values for reference test
#' @param actual Binary outcome vector (0/1)
#' @param direction Classification direction (">=" or "<=")
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence level (default 0.95)
#' @keywords internal
#' @return List with IDI, confidence intervals, and p-value
#' @export
bootstrapIDI <- function(new_values, ref_values, actual,
                         direction = ">=", n_boot = 1000,
                         conf_level = 0.95) {
    # Validate inputs
    n <- length(actual)
    if (length(new_values) != n || length(ref_values) != n) {
        stop("All input vectors must have the same length")
    }
    
    if (n_boot < 100) {
        warning("Low number of bootstrap iterations may produce unreliable results")
    }
    
    # Ensure actual is binary
    if (!all(actual %in% c(0, 1))) {
        stop("Actual values must be binary (0 or 1)")
    }
    
    # Original IDI calculation
    new_probs <- raw_to_prob(new_values, actual, direction, warn = FALSE)
    ref_probs <- raw_to_prob(ref_values, actual, direction, warn = FALSE)
    original_fit_warning <- length(attr(new_probs, "fit_warnings")) > 0L ||
        length(attr(ref_probs, "fit_warnings")) > 0L
    
    # Calculate discrimination slopes
    events <- actual == 1
    non_events <- actual == 0
    
    # Check for events and non-events
    if (sum(events) == 0 || sum(non_events) == 0) {
        warning("No events or non-events found in data")
        return(list(idi = NA, ci_lower = NA, ci_upper = NA, p_value = NA))
    }
    
    # Original IDI
    original_idi <- (mean(new_probs[events], na.rm = TRUE) -
                     mean(new_probs[non_events], na.rm = TRUE)) -
        (mean(ref_probs[events], na.rm = TRUE) -
         mean(ref_probs[non_events], na.rm = TRUE))
    
    # Bootstrap
    boot_idi <- numeric(n_boot)
    valid_boots <- 0
    warning_boots <- 0L
    
    for (i in 1:n_boot) {
        boot_idx <- sample(n, n, replace = TRUE)
        
        boot_new <- new_values[boot_idx]
        boot_ref <- ref_values[boot_idx]
        boot_actual <- actual[boot_idx]
        
        # Skip if no events or non-events in bootstrap sample
        if (sum(boot_actual == 1) == 0 || sum(boot_actual == 0) == 0) {
            boot_idi[i] <- NA
            next
        }
        
        # Calculate probabilities for bootstrap sample.
        #
        # The result is assigned from the value of tryCatch(), NOT from inside
        # the error handler. `boot_idi[i] <- NA` written in the handler assigns
        # into the handler function's own frame and never reaches this one, so
        # every failed replicate silently kept its preallocated 0.0 -- dragging
        # the estimate and the CI toward zero and counting in BOTH p-value
        # tails. It also meant the "many bootstraps failed" warning below could
        # never fire from this path.
        boot_idi[i] <- tryCatch({
            boot_new_probs <- raw_to_prob(boot_new, boot_actual, direction, warn = FALSE)
            boot_ref_probs <- raw_to_prob(boot_ref, boot_actual, direction, warn = FALSE)
            if (length(attr(boot_new_probs, "fit_warnings")) > 0L ||
                length(attr(boot_ref_probs, "fit_warnings")) > 0L) {
                warning_boots <- warning_boots + 1L
            }

            # Calculate IDI
            boot_events <- boot_actual == 1
            boot_non_events <- boot_actual == 0

            val <- (mean(boot_new_probs[boot_events], na.rm = TRUE) -
                    mean(boot_new_probs[boot_non_events], na.rm = TRUE)) -
                (mean(boot_ref_probs[boot_events], na.rm = TRUE) -
                 mean(boot_ref_probs[boot_non_events], na.rm = TRUE))
            if (is.finite(val)) {
                valid_boots <- valid_boots + 1
                val
            } else NA_real_
        }, error = function(e) NA_real_)
    }
    
    # Remove failed bootstrap samples
    boot_idi_valid <- boot_idi[!is.na(boot_idi)]
    
    if (length(boot_idi_valid) == 0) {
        # Nothing to summarise. Returning quantile(numeric(0)) and
        # mean(logical(0)) would hand back ci = NA and p = NaN with no
        # indication that every replicate had failed.
        warning("All bootstrap replicates failed; no confidence interval or p-value can be computed.",
                call. = FALSE)
        return(list(idi = original_idi, ci_lower = NA_real_, ci_upper = NA_real_,
                    p_value = NA_real_, n_valid_boots = 0,
                    fit_warning = original_fit_warning,
                    fit_warning_boots = warning_boots))
    }

    if (length(boot_idi_valid) < n_boot * 0.5) {
        warning(sprintf("Only %d of %d bootstrap replicates succeeded - results may be unreliable",
                        length(boot_idi_valid), n_boot), call. = FALSE)
    }
    if (original_fit_warning || warning_boots > 0L) {
        warning(sprintf(
            paste0(
                "Logistic calibration showed separation or non-convergence in the original ",
                "fit or %d of %d bootstrap replicates; IDI and its interval may be unstable."
            ),
            warning_boots, n_boot
        ), call. = FALSE)
    }
    
    # Calculate confidence intervals
    alpha <- 1 - conf_level
    ci_lower <- quantile(boot_idi_valid, alpha/2, na.rm = TRUE)
    ci_upper <- quantile(boot_idi_valid, 1 - alpha/2, na.rm = TRUE)
    
    # Calculate p-value (two-sided test for IDI = 0).
    #
    # (1 + count) / (B + 1) rather than count / B: a bootstrap p-value can never
    # legitimately be exactly 0, and the uncorrected form returned 0 whenever
    # every replicate fell on one side, implying impossible precision from a
    # finite number of replicates.
    B <- length(boot_idi_valid)
    p_value <- min(1, 2 * min(
        (1 + sum(boot_idi_valid <= 0)) / (B + 1),
        (1 + sum(boot_idi_valid >= 0)) / (B + 1)
    ))
    
    return(list(
        idi = original_idi,
        ci_lower = as.numeric(ci_lower),
        ci_upper = as.numeric(ci_upper),
        p_value = p_value,
        n_valid_boots = valid_boots,
        fit_warning = original_fit_warning,
        fit_warning_boots = warning_boots
    ))
}
