# Stage Migration Analysis - Discrimination Metrics
#
# Advanced discrimination metrics for staging system comparison including
# NRI (Net Reclassification Improvement), IDI (Integrated Discrimination
# Improvement), and time-dependent metrics.

# Source: Functions moved and refactored from stagemigration_helpers.R

# =============================================================================
# C-INDEX AND CONCORDANCE METRICS
# =============================================================================

#' Calculate Advanced Concordance Metrics
#'
#' @description
#' Comprehensive concordance comparison with proper handling of correlated
#' staging systems (same patients, two measurements).
#'
#' @param data Data frame with complete cases
#' @param old_stage Old staging variable name
#' @param new_stage New staging variable name
#' @param time_var Survival time variable name
#' @param event_var Binary event indicator variable name
#' @param perform_bootstrap Logical, whether to perform bootstrap validation
#' @param bootstrap_reps Number of bootstrap repetitions
#' @param checkpoint_callback Optional function to call for progress updates
#' @return List with concordance metrics and comparisons
#' @keywords internal
#' @importFrom survival coxph Surv concordance
#' @importFrom stats cor complete.cases pnorm AIC BIC pchisq
stagemigration_calculateConcordance <- function(data, old_stage, new_stage,
                                                time_var, event_var,
                                                perform_bootstrap = FALSE,
                                                bootstrap_reps = 1000,
                                                checkpoint_callback = NULL) {
    # Build formulas with escaped variable names
    old_formula <- stagemigration_buildFormula(time_var, event_var, old_stage)
    new_formula <- stagemigration_buildFormula(time_var, event_var, new_stage)

    result <- stagemigration_safeExecute(
        expr = {
            if (!is.null(checkpoint_callback)) checkpoint_callback()

            # Fit Cox models
            old_cox <- survival::coxph(old_formula, data = data)
            new_cox <- survival::coxph(new_formula, data = data)

            # Calculate concordance indices
            old_concordance <- survival::concordance(old_cox)
            new_concordance <- survival::concordance(new_cox)

            old_c <- old_concordance$concordance
            new_c <- new_concordance$concordance
            old_var <- old_concordance$var
            new_var <- new_concordance$var

            # Calculate improvement
            c_improvement <- new_c - old_c
            c_improvement_pct <- if (old_c > 0) (c_improvement / old_c) * 100 else NA

            # ===================================================================
            # Correlation correction for dependent C-indices
            # (Same patients measured with two staging systems)
            # ===================================================================

            old_lp <- predict(old_cox, type = "lp")
            new_lp <- predict(new_cox, type = "lp")

            # Calculate correlation between linear predictors
            valid_idx <- complete.cases(old_lp, new_lp)
            r_correlation <- if (sum(valid_idx) > 10) {
                cor(old_lp[valid_idx], new_lp[valid_idx], method = "spearman")
            } else {
                0
            }
            r_correlation <- max(-1, min(1, if(is.na(r_correlation)) 0 else r_correlation))

            # Calculate SE of difference accounting for correlation
            diff_se <- NA
            p_value <- NA

            if (old_var >= 0 && new_var >= 0) {
                old_se <- sqrt(old_var)
                new_se <- sqrt(new_var)

                # Var(X - Y) = Var(X) + Var(Y) - 2*Cov(X,Y)
                # Cov(X,Y) ≈ r * SE(X) * SE(Y)
                covariance_term <- 2 * r_correlation * old_se * new_se
                diff_var <- old_var + new_var - covariance_term
                diff_var <- max(0, diff_var)  # Ensure non-negative

                diff_se <- sqrt(diff_var)

                # Z-test for difference
                z_stat <- if (diff_se > 0) c_improvement / diff_se else NA
                p_value <- if (!is.na(z_stat)) 2 * (1 - pnorm(abs(z_stat))) else NA
            }

            # ===================================================================
            # Bootstrap validation (if requested)
            # ===================================================================

            c_bootstrap <- NULL
            c_improvement_ci_lower <- NA
            c_improvement_ci_upper <- NA

            if (perform_bootstrap) {
                c_bootstrap <- stagemigration_bootstrapConcordance(
                    data = data,
                    old_formula = old_formula,
                    new_formula = new_formula,
                    n_boot = bootstrap_reps,
                    checkpoint_callback = checkpoint_callback
                )

                if (!is.null(c_bootstrap)) {
                    p_value <- c_bootstrap$p_value
                    diff_se <- c_bootstrap$se
                    c_improvement_ci_lower <- c_bootstrap$ci_lower
                    c_improvement_ci_upper <- c_bootstrap$ci_upper
                }
            } else {
                # Asymptotic CI
                if (!is.na(c_improvement) && !is.na(diff_se) && diff_se > 0) {
                    c_improvement_ci_lower <- c_improvement - 1.96 * diff_se
                    c_improvement_ci_upper <- c_improvement + 1.96 * diff_se
                }
            }

            # ===================================================================
            # Model fit statistics
            # ===================================================================

            aic_old <- AIC(old_cox)
            aic_new <- AIC(new_cox)
            aic_improvement <- aic_old - aic_new

            bic_old <- BIC(old_cox)
            bic_new <- BIC(new_cox)
            bic_improvement <- bic_old - bic_new

            # ===================================================================
            # Likelihood ratio test for nested models
            # ===================================================================

            lr_test <- tryCatch({
                # Build combined formula
                combined_formula <- stagemigration_buildFormula(
                    time_var, event_var, c(old_stage, new_stage)
                )
                combined_cox <- survival::coxph(combined_formula, data = data)

                # LR test: new vs combined
                lr_new <- 2 * (combined_cox$loglik[2] - new_cox$loglik[2])
                df_new <- length(coef(combined_cox)) - length(coef(new_cox))
                p_new <- pchisq(lr_new, df_new, lower.tail = FALSE)

                # LR test: old vs combined
                lr_old <- 2 * (combined_cox$loglik[2] - old_cox$loglik[2])

                list(
                    lr_stat = lr_new - lr_old,
                    df = df_new,
                    p_value = p_new
                )
            }, error = function(e) {
                list(lr_stat = NA, df = NA, p_value = NA)
            })

            # ===================================================================
            # Individual model LR tests
            # ===================================================================

            individual_lr_stats <- tryCatch({
                old_summary <- summary(old_cox)
                new_summary <- summary(new_cox)
                list(
                    old_lr_chi2 = old_summary$logtest["test"],
                    old_lr_df = old_summary$logtest["df"],
                    old_lr_p = old_summary$logtest["pvalue"],
                    new_lr_chi2 = new_summary$logtest["test"],
                    new_lr_df = new_summary$logtest["df"],
                    new_lr_p = new_summary$logtest["pvalue"]
                )
            }, error = function(e) NULL)

            # ===================================================================
            # Return comprehensive results
            # ===================================================================

            list(
                old_cox = old_cox,
                new_cox = new_cox,
                old_concordance = old_concordance,
                new_concordance = new_concordance,
                old_c = old_c,
                new_c = new_c,
                old_c_se = if (old_var >= 0) sqrt(old_var) else NA,
                new_c_se = if (new_var >= 0) sqrt(new_var) else NA,
                c_improvement = c_improvement,
                c_improvement_pct = c_improvement_pct,
                c_improvement_se = diff_se,
                c_improvement_p = p_value,
                c_improvement_ci_lower = c_improvement_ci_lower,
                c_improvement_ci_upper = c_improvement_ci_upper,
                correlation = r_correlation,
                c_bootstrap = c_bootstrap,
                aic_old = aic_old,
                aic_new = aic_new,
                aic_improvement = aic_improvement,
                bic_old = bic_old,
                bic_new = bic_new,
                bic_improvement = bic_improvement,
                lr_test = lr_test,
                individual_lr_stats = individual_lr_stats
            )
        },
        errorReturn = list(error = "C-index calculation failed"),
        errorMessage = "C-index calculation",
        context = "stagemigration_calculateConcordance"
    )

    return(result)
}

#' Bootstrap Concordance Comparison
#'
#' @description
#' Bootstrap validation of C-index difference accounting for correlation
#' between staging systems (same patients).
#'
#' @keywords internal
stagemigration_bootstrapConcordance <- function(data, old_formula, new_formula,
                                                n_boot = 1000,
                                                checkpoint_callback = NULL) {
    tryCatch({
        n <- nrow(data)
        c_diffs <- numeric(n_boot)

        # Calculate original difference
        old_cox_orig <- survival::coxph(old_formula, data = data)
        new_cox_orig <- survival::coxph(new_formula, data = data)
        c_diff_orig <- survival::concordance(new_cox_orig)$concordance -
                       survival::concordance(old_cox_orig)$concordance

        # Bootstrap sampling
        for (i in 1:n_boot) {
            if (!is.null(checkpoint_callback) && i %% 50 == 1) {
                checkpoint_callback()
            }

            # Resample with replacement
            boot_idx <- sample(1:n, n, replace = TRUE)
            boot_data <- data[boot_idx, ]

            # Fit models on bootstrap sample (suppress convergence warnings)
            old_c_boot <- tryCatch({
                m <- suppressWarnings(survival::coxph(old_formula, data = boot_data))
                survival::concordance(m)$concordance
            }, error = function(e) NA)

            new_c_boot <- tryCatch({
                m <- suppressWarnings(survival::coxph(new_formula, data = boot_data))
                survival::concordance(m)$concordance
            }, error = function(e) NA)

            if (!is.na(old_c_boot) && !is.na(new_c_boot)) {
                c_diffs[i] <- new_c_boot - old_c_boot
            } else {
                c_diffs[i] <- NA
            }
        }

        # Remove failed bootstrap samples
        c_diffs <- c_diffs[!is.na(c_diffs)]

        if (length(c_diffs) < 50) {
            return(list(p_value = NA, se = NA, ci_lower = NA, ci_upper = NA))
        }

        # P-value: two-sided test
        p_value <- 2 * min(mean(c_diffs <= 0), mean(c_diffs >= 0))

        # Confidence interval: percentile method
        ci_lower <- quantile(c_diffs, 0.025, na.rm = TRUE)
        ci_upper <- quantile(c_diffs, 0.975, na.rm = TRUE)

        list(
            c_diff = c_diff_orig,
            p_value = p_value,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            se = sd(c_diffs, na.rm = TRUE),
            n_successful_boots = length(c_diffs)
        )
    }, error = function(e) {
        list(p_value = NA, se = NA, ci_lower = NA, ci_upper = NA)
    })
}

# NOTE: NRI and IDI functions are available in stagemigration_helpers.R
# This file focuses on concordance metrics only to avoid duplication.