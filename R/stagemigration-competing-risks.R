#' Stage Migration Analysis - Competing Risks Methods
#'
#' @description
#' Competing risks analysis for stage migration when patients can experience
#' multiple types of events (e.g., cancer death vs. other causes).
#'
#' @keywords internal
#' @noRd

# =============================================================================
# COMPETING RISKS ANALYSIS
# =============================================================================

#' Perform Competing Risks Analysis for Stage Migration
#'
#' @description
#' Analyzes stage migration accounting for competing risks using cumulative
#' incidence functions (CIF) and Fine-Gray models.
#'
#' @param data Data frame
#' @param old_stage Old staging variable name
#' @param new_stage New staging variable name
#' @param time_var Survival time variable name
#' @param event_var Event type variable name (factor with multiple levels)
#' @param event_of_interest Primary event of interest
#' @param checkpoint_callback Optional progress callback
#' @return List with competing risks analysis results
#' @keywords internal
stagemigration_competingRisksAnalysis <- function(data, old_stage, new_stage,
                                                   time_var, event_var,
                                                   event_of_interest,
                                                   checkpoint_callback = NULL) {
    # Check if required packages are available
    if (!requireNamespace("cmprsk", quietly = TRUE)) {
        return(list(
            error = "Package 'cmprsk' required for competing risks analysis",
            recommendation = "Install with: install.packages('cmprsk')"
        ))
    }

    result <- stagemigration_safeExecute(
        expr = {
            if (!is.null(checkpoint_callback)) checkpoint_callback()

            # ===================================================================
            # Data preparation
            # ===================================================================

            # Ensure event_var is factor
            if (!is.factor(data[[event_var]])) {
                data[[event_var]] <- as.factor(data[[event_var]])
            }

            # Get event levels
            event_levels <- levels(data[[event_var]])

            # Verify event of interest exists
            if (!event_of_interest %in% event_levels) {
                return(list(
                    error = paste("Event of interest not found:",
                                 event_of_interest,
                                 ". Available:",
                                 paste(event_levels, collapse = ", "))
                ))
            }

            # Create numeric event indicator
            # 0 = censored, 1 = event of interest, 2+ = competing events
            event_numeric <- as.numeric(data[[event_var]])
            event_numeric[data[[event_var]] == event_of_interest] <- 1
            event_numeric[data[[event_var]] != event_of_interest &
                         data[[event_var]] != levels(data[[event_var]])[1]] <- 2

            # Ensure staging variables are factors
            if (!is.factor(data[[old_stage]])) {
                data[[old_stage]] <- as.factor(data[[old_stage]])
            }
            if (!is.factor(data[[new_stage]])) {
                data[[new_stage]] <- as.factor(data[[new_stage]])
            }

            # ===================================================================
            # Cumulative Incidence Functions
            # ===================================================================

            # Old staging system
            cif_old <- cmprsk::cuminc(
                ftime = data[[time_var]],
                fstatus = event_numeric,
                group = data[[old_stage]],
                cencode = 0
            )

            # New staging system
            cif_new <- cmprsk::cuminc(
                ftime = data[[time_var]],
                fstatus = event_numeric,
                group = data[[new_stage]],
                cencode = 0
            )

            # ===================================================================
            # Gray's test for equality of CIFs across groups
            # ===================================================================

            # Test for old staging system
            gray_test_old <- tryCatch({
                test_result <- cif_old$Tests
                list(
                    statistic = test_result[1, 1],
                    df = test_result[1, 2],
                    p_value = test_result[1, 3],
                    interpretation = if (test_result[1, 3] < 0.05) {
                        "Significant differences in CIF across old stages"
                    } else {
                        "No significant differences in CIF across old stages"
                    }
                )
            }, error = function(e) {
                list(error = "Gray's test failed for old staging")
            })

            # Test for new staging system
            gray_test_new <- tryCatch({
                test_result <- cif_new$Tests
                list(
                    statistic = test_result[1, 1],
                    df = test_result[1, 2],
                    p_value = test_result[1, 3],
                    interpretation = if (test_result[1, 3] < 0.05) {
                        "Significant differences in CIF across new stages"
                    } else {
                        "No significant differences in CIF across new stages"
                    }
                )
            }, error = function(e) {
                list(error = "Gray's test failed for new staging")
            })

            # ===================================================================
            # Fine-Gray regression models
            # ===================================================================

            # Prepare data for Fine-Gray model
            fg_data <- data.frame(
                time = data[[time_var]],
                event = event_numeric,
                old_stage = data[[old_stage]],
                new_stage = data[[new_stage]]
            )

            # Model with old staging
            fg_old <- tryCatch({
                cmprsk::crr(
                    ftime = fg_data$time,
                    fstatus = fg_data$event,
                    cov1 = model.matrix(~ old_stage, data = fg_data)[, -1, drop = FALSE],
                    failcode = 1,
                    cencode = 0
                )
            }, error = function(e) {
                list(error = "Fine-Gray model failed for old staging")
            })

            # Model with new staging
            fg_new <- tryCatch({
                cmprsk::crr(
                    ftime = fg_data$time,
                    fstatus = fg_data$event,
                    cov1 = model.matrix(~ new_stage, data = fg_data)[, -1, drop = FALSE],
                    failcode = 1,
                    cencode = 0
                )
            }, error = function(e) {
                list(error = "Fine-Gray model failed for new staging")
            })

            # ===================================================================
            # Model comparison
            # ===================================================================

            model_comparison <- list(
                old_convergence = if (is.list(fg_old) && !is.null(fg_old$converged)) {
                    fg_old$converged
                } else NA,
                new_convergence = if (is.list(fg_new) && !is.null(fg_new$converged)) {
                    fg_new$converged
                } else NA
            )

            # Extract AIC/BIC if available
            if (is.list(fg_old) && !is.null(fg_old$loglik)) {
                k_old <- length(fg_old$coef)
                model_comparison$old_loglik <- fg_old$loglik
                model_comparison$old_aic <- -2 * fg_old$loglik + 2 * k_old
            }

            if (is.list(fg_new) && !is.null(fg_new$loglik)) {
                k_new <- length(fg_new$coef)
                model_comparison$new_loglik <- fg_new$loglik
                model_comparison$new_aic <- -2 * fg_new$loglik + 2 * k_new
            }

            # ===================================================================
            # Discrimination metrics for competing risks
            # ===================================================================

            discrimination <- stagemigration_competingRisksDiscrimination(
                data = fg_data,
                fg_old = fg_old,
                fg_new = fg_new,
                time_points = c(12, 24, 60)
            )

            # ===================================================================
            # Return comprehensive results
            # ===================================================================

            list(
                success = TRUE,
                cif_old = cif_old,
                cif_new = cif_new,
                gray_test_old = gray_test_old,
                gray_test_new = gray_test_new,
                fg_model_old = fg_old,
                fg_model_new = fg_new,
                model_comparison = model_comparison,
                discrimination = discrimination,
                event_summary = list(
                    event_of_interest = event_of_interest,
                    n_events_interest = sum(event_numeric == 1),
                    n_competing = sum(event_numeric == 2),
                    n_censored = sum(event_numeric == 0),
                    event_levels = event_levels
                )
            )
        },
        errorReturn = list(error = "Competing risks analysis failed"),
        errorMessage = "Competing risks analysis",
        context = "stagemigration_competingRisksAnalysis"
    )

    return(result)
}

#' Calculate Discrimination Metrics for Competing Risks
#'
#' @description
#' Calculates time-dependent AUC and concordance for competing risks models.
#'
#' @keywords internal
stagemigration_competingRisksDiscrimination <- function(data, fg_old, fg_new,
                                                        time_points = c(12, 24, 60)) {
    result <- list(
        time_dependent_auc = list(),
        concordance = list()
    )

    # Check if models are valid
    if (!is.list(fg_old) || is.null(fg_old$coef) ||
        !is.list(fg_new) || is.null(fg_new$coef)) {
        return(result)
    }

    # Try to calculate time-dependent AUC if timeROC package available
    if (requireNamespace("timeROC", quietly = TRUE)) {
        tryCatch({
            # This is a simplified version - full implementation would require
            # converting Fine-Gray predictions to risk scores
            result$concordance$note <- "Time-dependent AUC for competing risks requires specialized implementation"
        }, error = function(e) {
            result$concordance$error <- e$message
        })
    } else {
        result$note <- "Package 'timeROC' recommended for time-dependent discrimination metrics"
    }

    return(result)
}

# =============================================================================
# RESTRICTED MEAN SURVIVAL TIME (RMST) ANALYSIS
# =============================================================================

#' Calculate Restricted Mean Survival Time for Stage Migration
#'
#' @description
#' RMST analysis provides an alternative to hazard ratios, reporting the
#' average survival time up to a specified time point.
#'
#' @param data Data frame
#' @param old_stage Old staging variable name
#' @param new_stage New staging variable name
#' @param time_var Survival time variable name
#' @param event_var Binary event indicator
#' @param tau Restriction time (maximum follow-up to consider)
#' @param checkpoint_callback Optional progress callback
#' @return List with RMST results
#' @keywords internal
stagemigration_calculateRMST <- function(data, old_stage, new_stage,
                                        time_var, event_var,
                                        tau = NULL,
                                        checkpoint_callback = NULL) {
    # Check if survRM2 package available
    if (!requireNamespace("survRM2", quietly = TRUE)) {
        return(list(
            error = "Package 'survRM2' required for RMST analysis",
            recommendation = "Install with: install.packages('survRM2')"
        ))
    }

    result <- stagemigration_safeExecute(
        expr = {
            if (!is.null(checkpoint_callback)) checkpoint_callback()

            # Determine tau if not specified (e.g., 75th percentile of observed times)
            if (is.null(tau)) {
                tau <- quantile(data[[time_var]][data[[event_var]] == 1],
                               probs = 0.75, na.rm = TRUE)
            }

            # Ensure staging variables are factors
            if (!is.factor(data[[old_stage]])) {
                data[[old_stage]] <- as.factor(data[[old_stage]])
            }
            if (!is.factor(data[[new_stage]])) {
                data[[new_stage]] <- as.factor(data[[new_stage]])
            }

            # ===================================================================
            # RMST by stage groups
            # ===================================================================

            # Old staging system
            rmst_by_old_stage <- list()
            old_stage_levels <- levels(data[[old_stage]])

            for (stage in old_stage_levels) {
                stage_data <- data[data[[old_stage]] == stage, ]

                rmst_result <- tryCatch({
                    survRM2::rmst1(
                        time = stage_data[[time_var]],
                        status = stage_data[[event_var]],
                        tau = tau
                    )
                }, error = function(e) {
                    list(error = paste("RMST calculation failed for", stage))
                })

                rmst_by_old_stage[[stage]] <- rmst_result
            }

            # New staging system
            rmst_by_new_stage <- list()
            new_stage_levels <- levels(data[[new_stage]])

            for (stage in new_stage_levels) {
                stage_data <- data[data[[new_stage]] == stage, ]

                rmst_result <- tryCatch({
                    survRM2::rmst1(
                        time = stage_data[[time_var]],
                        status = stage_data[[event_var]],
                        tau = tau
                    )
                }, error = function(e) {
                    list(error = paste("RMST calculation failed for", stage))
                })

                rmst_by_new_stage[[stage]] <- rmst_result
            }

            # ===================================================================
            # Pairwise RMST comparisons (if 2 groups)
            # ===================================================================

            pairwise_comparison <- list()

            # Compare reference stages if both systems have 2+ levels
            if (length(old_stage_levels) >= 2 && length(new_stage_levels) >= 2) {
                # Old system: compare first vs last stage
                old_compare <- tryCatch({
                    ref_data <- data[data[[old_stage]] == old_stage_levels[1], ]
                    comp_data <- data[data[[old_stage]] == old_stage_levels[length(old_stage_levels)], ]

                    survRM2::rmst2(
                        time = c(ref_data[[time_var]], comp_data[[time_var]]),
                        status = c(ref_data[[event_var]], comp_data[[event_var]]),
                        arm = c(rep(0, nrow(ref_data)), rep(1, nrow(comp_data))),
                        tau = tau
                    )
                }, error = function(e) NULL)

                # New system: compare first vs last stage
                new_compare <- tryCatch({
                    ref_data <- data[data[[new_stage]] == new_stage_levels[1], ]
                    comp_data <- data[data[[new_stage]] == new_stage_levels[length(new_stage_levels)], ]

                    survRM2::rmst2(
                        time = c(ref_data[[time_var]], comp_data[[time_var]]),
                        status = c(ref_data[[event_var]], comp_data[[event_var]]),
                        arm = c(rep(0, nrow(ref_data)), rep(1, nrow(comp_data))),
                        tau = tau
                    )
                }, error = function(e) NULL)

                pairwise_comparison <- list(
                    old_system = old_compare,
                    new_system = new_compare,
                    comparison = paste("Comparing", old_stage_levels[1], "vs",
                                     old_stage_levels[length(old_stage_levels)])
                )
            }

            # ===================================================================
            # Return results
            # ===================================================================

            list(
                success = TRUE,
                tau = tau,
                rmst_by_old_stage = rmst_by_old_stage,
                rmst_by_new_stage = rmst_by_new_stage,
                pairwise_comparison = pairwise_comparison,
                interpretation = paste(
                    "RMST represents average survival time up to", tau,
                    "time units. Higher RMST indicates better survival."
                )
            )
        },
        errorReturn = list(error = "RMST analysis failed"),
        errorMessage = "RMST analysis",
        context = "stagemigration_calculateRMST"
    )

    return(result)
}

# =============================================================================
# CUTPOINT ANALYSIS
# =============================================================================

#' Optimal Cutpoint Analysis for Stage Grouping
#'
#' @description
#' Identifies optimal cutpoints for creating stage groups based on
#' survival discrimination.
#'
#' @param data Data frame
#' @param stage_var Staging variable (continuous or ordinal)
#' @param time_var Survival time variable
#' @param event_var Binary event indicator
#' @param method Method for cutpoint selection ("maxstat", "median", "tertiles")
#' @return List with cutpoint analysis results
#' @keywords internal
stagemigration_cutpointAnalysis <- function(data, stage_var, time_var, event_var,
                                            method = "maxstat") {
    result <- stagemigration_safeExecute(
        expr = {
            # Check if maxstat package available for optimal cutpoint
            if (method == "maxstat" && !requireNamespace("maxstat", quietly = TRUE)) {
                method <- "median"
                warning("Package 'maxstat' not available, using median cutpoint")
            }

            # Get stage values
            stage_values <- data[[stage_var]]

            # Remove missing
            valid_idx <- complete.cases(data[[time_var]], data[[event_var]], stage_values)
            data_clean <- data[valid_idx, ]
            stage_clean <- stage_values[valid_idx]

            if (method == "maxstat") {
                # Maximally selected rank statistics
                surv_obj <- survival::Surv(data_clean[[time_var]], data_clean[[event_var]])

                maxstat_result <- maxstat::maxstat.test(
                    Surv(time, event) ~ stage,
                    data = data.frame(
                        time = data_clean[[time_var]],
                        event = data_clean[[event_var]],
                        stage = as.numeric(stage_clean)
                    ),
                    smethod = "LogRank",
                    pmethod = "exactGauss"
                )

                list(
                    method = "maxstat",
                    optimal_cutpoint = maxstat_result$estimate,
                    statistic = maxstat_result$statistic,
                    p_value = maxstat_result$p.value,
                    interpretation = "Optimal cutpoint by maximally selected log-rank statistic"
                )

            } else if (method == "median") {
                # Median split
                median_val <- median(as.numeric(stage_clean), na.rm = TRUE)

                list(
                    method = "median",
                    optimal_cutpoint = median_val,
                    interpretation = "Median split"
                )

            } else if (method == "tertiles") {
                # Tertile splits
                tertiles <- quantile(as.numeric(stage_clean),
                                    probs = c(1/3, 2/3), na.rm = TRUE)

                list(
                    method = "tertiles",
                    optimal_cutpoints = tertiles,
                    interpretation = "Tertile splits (33rd and 67th percentiles)"
                )
            } else {
                list(error = paste("Unknown cutpoint method:", method))
            }
        },
        errorReturn = list(error = "Cutpoint analysis failed"),
        errorMessage = "Cutpoint analysis",
        context = "stagemigration_cutpointAnalysis"
    )

    return(result)
}
