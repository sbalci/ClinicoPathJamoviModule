#' Advanced Metrics Calculation for Stage Migration
#' @importFrom survival coxph Surv concordance survfit
#' @importFrom stats cor sd quantile complete.cases pnorm pchisq var approx AIC BIC
#' @importFrom Hmisc rcorrp.cens
#' @return A named list of stage-migration discrimination and calibration
#'   metrics comparing the old and new staging Cox models (fitted models,
#'   concordance indices, C-index improvement with SE/CI/bootstrap, AIC/BIC
#'   improvements, likelihood-ratio and linear-trend tests, pseudo R-squared,
#'   and individual model LR statistics); on failure a list with a single
#'   `error` element containing the error message.
#' @keywords internal

# Two-sided normal critical value from the user's confidenceLevel option.
# Falls back to 95% when the option is unavailable (e.g. a direct helper call).
stagemigration_zcrit <- function(options) {
    cl <- tryCatch(options$confidenceLevel, error = function(e) NULL)
    if (is.null(cl) || !is.finite(cl) || cl <= 0 || cl >= 1) cl <- 0.95
    stats::qnorm(1 - (1 - cl) / 2)
}

# Matching two-sided quantile probabilities for percentile bootstrap intervals.
stagemigration_ciprobs <- function(options) {
    cl <- tryCatch(options$confidenceLevel, error = function(e) NULL)
    if (is.null(cl) || !is.finite(cl) || cl <= 0 || cl >= 1) cl <- 0.95
    c((1 - cl) / 2, 1 - (1 - cl) / 2)
}

stagemigration_calculateAdvancedMetrics <- function(data, options, checkpoint_callback = NULL) {
    # Advanced discrimination and calibration metrics with comprehensive error handling
    
    old_stage <- options$oldStage
    new_stage <- options$newStage
    time_var <- options$survivalTime
    event_var <- "event_binary"

    # Fit Cox models
    old_formula <- stats::as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", old_stage, "`", sep=""))
    new_formula <- stats::as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", new_stage, "`", sep=""))

    tryCatch({
        if (!is.null(checkpoint_callback)) checkpoint_callback()
        
        # Fit old Cox model
        old_cox <- survival::coxph(old_formula, data = data)
        # Fit new Cox model
        new_cox <- survival::coxph(new_formula, data = data)

        # Calculate concordance indices (Standard)
        old_concordance <- survival::concordance(old_cox)
        new_concordance <- survival::concordance(new_cox)
        
        old_c <- old_concordance$concordance
        new_c <- new_concordance$concordance
        old_var <- old_concordance$var
        new_var <- new_concordance$var

        # Calculate improvement
        c_improvement <- new_c - old_c
        c_improvement_pct <- if (old_c > 0) (c_improvement / old_c) * 100 else NA

        # --- Paired variance of the C-index difference ---
        # Both C-indices rank the same patients, so the difference needs their covariance.
        # survival::concordance(old_cox, new_cox) returns the joint variance matrix of the two
        # estimates (the same estimator as the per-model C-index above). The former code derived
        # the covariance from the Spearman correlation of the linear predictors, which is not a
        # variance of the concordance statistic: on the bundled cohort it reported p < 0.001 where
        # the paired test gives p = 0.002.
        diff_se <- NA
        p_value <- NA
        paired <- tryCatch(survival::concordance(old_cox, new_cox), error = function(e) NULL)
        if (!is.null(paired) && is.matrix(paired$var) && all(dim(paired$var) == 2)) {
            diff_var <- drop(c(-1, 1) %*% paired$var %*% c(-1, 1))
            if (is.finite(diff_var) && diff_var > 0) {
                diff_se <- sqrt(diff_var)
                p_value <- 2 * stats::pnorm(-abs(c_improvement / diff_se))
            }
        }

        use_bootstrap <- options$analysisType %in% c("comprehensive", "publication") && options$performBootstrap
        c_bootstrap <- NULL
        c_improvement_ci_lower <- NA
        c_improvement_ci_upper <- NA

        if (use_bootstrap) {
            c_bootstrap <- stagemigration_compareBootstrapCIndex(
                data, old_stage, new_stage, time_var, event_var,
                n_boot = options$bootstrapReps %||% 200,
                checkpoint_callback = checkpoint_callback,
                options = options
            )
            p_value <- c_bootstrap$p_value
            diff_se <- c_bootstrap$se
            c_improvement_ci_lower <- c_bootstrap$ci_lower
            c_improvement_ci_upper <- c_bootstrap$ci_upper
        } else { 
             if (!is.na(c_improvement) && !is.na(diff_se)) {
                c_improvement_ci_lower <- c_improvement - stagemigration_zcrit(options) * diff_se
                c_improvement_ci_upper <- c_improvement + stagemigration_zcrit(options) * diff_se
            }
        }

        aic_old <- AIC(old_cox)
        aic_new <- AIC(new_cox)
        aic_improvement <- aic_old - aic_new
        bic_old <- BIC(old_cox)
        bic_new <- BIC(new_cox)
        bic_improvement <- bic_old - bic_new

        # Nested likelihood-ratio tests through the Cox model that holds BOTH classifications.
        # The two staging systems are not nested in each other, so no LR test compares them
        # directly. The former code reported lr_new - lr_old, which reduces to
        # 2 * (logLik(old) - logLik(new)) and is negative whenever the new system fits better, next
        # to the p-value of a different test. Each system is instead tested for the prognostic
        # information it adds to the other; p_value is "new staging adds to the original".
        lr_test <- tryCatch({
            combined_formula <- stats::as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `",
                                                old_stage, "` + `", new_stage, "`", sep=""))
            combined_cox <- survival::coxph(combined_formula, data = data)
            n_coef <- function(fit) sum(!is.na(stats::coef(fit)))
            nested <- function(reduced) {
                stat <- 2 * (combined_cox$loglik[2] - reduced$loglik[2])
                df <- n_coef(combined_cox) - n_coef(reduced)
                list(stat = stat, df = df, p = if (df > 0) stats::pchisq(stat, df, lower.tail = FALSE) else NA_real_)
            }
            new_adds <- nested(old_cox)
            old_adds <- nested(new_cox)
            list(new_adds = new_adds, old_adds = old_adds, p_value = new_adds$p)
        }, error = function(e) {
            list(new_adds = NULL, old_adds = NULL, p_value = NA)
        })

        linear_trend_test <- stagemigration_calculateLinearTrendTest(data, old_stage, new_stage, time_var, event_var)

        pseudo_r2 <- NULL
        if (options$calculatePseudoR2) {
            pseudo_r2 <- stagemigration_calculatePseudoR2(old_cox, new_cox, data, options)
        }
        
         individual_lr_stats <- tryCatch({
             old_s <- summary(old_cox); new_s <- summary(new_cox)
             list(
                 old_lr_chi2 = old_s$logtest["test"], old_lr_df = old_s$logtest["df"], old_lr_p = old_s$logtest["pvalue"],
                 new_lr_chi2 = new_s$logtest["test"], new_lr_df = new_s$logtest["df"], new_lr_p = new_s$logtest["pvalue"]
             )
         }, error = function(e) NULL)

        res <- list(
            old_cox = old_cox, new_cox = new_cox,
            old_concordance = old_concordance, new_concordance = new_concordance,
            c_improvement = c_improvement, c_improvement_pct = c_improvement_pct,
            c_improvement_se = diff_se, c_improvement_p = p_value,
            c_improvement_ci_lower = c_improvement_ci_lower, c_improvement_ci_upper = c_improvement_ci_upper,
            c_bootstrap = c_bootstrap,
            aic_old = aic_old, aic_new = aic_new, aic_improvement = aic_improvement,
            bic_old = bic_old, bic_new = bic_new, bic_improvement = bic_improvement,
            lr_test = lr_test,
            linear_trend_test = linear_trend_test,
            individual_lr_stats = individual_lr_stats,
            pseudo_r2 = pseudo_r2
        )
        return(res)

    }, error = function(e) {
        return(list(error = e$message))
    })
}

stagemigration_compareBootstrapCIndex <- function(data, old_stage, new_stage, time_var, event_var, n_boot = 200, checkpoint_callback = NULL, options = NULL) {
    # Bootstrap comparison of C-indices for correlated data
    tryCatch({
        n <- nrow(data)
        c_diffs <- numeric(n_boot)

        old_formula <- stats::as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", old_stage, "`", sep=""))
        new_formula <- stats::as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", new_stage, "`", sep=""))

        # Calculate original difference
        old_cox_orig <- survival::coxph(old_formula, data = data)
        new_cox_orig <- survival::coxph(new_formula, data = data)
        c_diff_orig <- survival::concordance(new_cox_orig)$concordance - survival::concordance(old_cox_orig)$concordance

        for (i in 1:n_boot) {
            if (!is.null(checkpoint_callback) && (i %% 25 == 0 || i == 1)) checkpoint_callback()
            
            boot_idx <- sample(1:n, n, replace = TRUE)
            boot_data <- data[boot_idx, ]
            
            # Fit models on bootstrap sample
            # Suppress warnings for convergence in bootstrap
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
        
        c_diffs <- c_diffs[!is.na(c_diffs)]
        
        if (length(c_diffs) < 50) return(list(p_value=NA, se=NA, ci_lower=NA, ci_upper=NA))

        # P-value (Two-sided)
        # Probability that 0 is outside the distribution relative to the mean difference?
        # Or simply: 2 * min(P(diff > 0), P(diff < 0))
        # <= and >= both count exact zeros, which are common when C-index differences
        # come from a few discrete stage levels, so this could return p > 1 (e.g. 1.4).
        # Split the ties between the two tails and cap at 1.
        p_zero <- mean(c_diffs == 0)
        p_value <- min(1, 2 * min(mean(c_diffs < 0) + 0.5 * p_zero,
                                  mean(c_diffs > 0) + 0.5 * p_zero))
        
        ci_probs <- stagemigration_ciprobs(options)
        ci_lower <- quantile(c_diffs, ci_probs[1], na.rm = TRUE)
        ci_upper <- quantile(c_diffs, ci_probs[2], na.rm = TRUE)

        list(
            c_diff = c_diff_orig,
            p_value = p_value,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            se = sd(c_diffs, na.rm = TRUE)
        )
    }, error = function(e) {
        list(p_value = NA, se = NA, ci_lower = NA, ci_upper = NA)
    })
}

stagemigration_calculateLinearTrendTest <- function(data, old_stage, new_stage, time_var, event_var) {
    tryCatch({
        surv_obj <- survival::Surv(data[[time_var]], data[[event_var]])
        
        .calc <- function(stage_col) {
            stages <- sort(unique(data[[stage_col]]))
            if (length(stages) < 3) return(list(stat = NA, p_value = NA, interpretation = "Need >= 3 stages"))
            
            stage_scores <- match(data[[stage_col]], stages)
            trend_data <- data.frame(surv_obj = surv_obj, stage_scores = stage_scores)
            trend_data <- trend_data[!is.na(trend_data$stage_scores),]
            
            if (nrow(trend_data) < 10) return(list(stat = NA, p_value = NA))
            
            trend_cox <- survival::coxph(surv_obj ~ stage_scores, data = trend_data)
            s <- summary(trend_cox)
            # The renderer reads coefficient, n_stages and interpretation. They were never
            # set here, so the Linear Trend Test table always showed two blank columns and
            # "Unable to interpret" on both rows.
            coefval <- unname(stats::coef(trend_cox)[1])
            pval <- unname(s$waldtest["pvalue"])
            list(
                stat = unname(s$waldtest["test"]),
                p_value = pval,
                coefficient = coefval,
                n_stages = length(stages),
                interpretation = if (!is.finite(pval)) {
                    "Not estimable"
                } else if (pval >= 0.05) {
                    "No significant monotonic trend across stages"
                } else if (coefval > 0) {
                    "Significant increasing hazard across ordered stages"
                } else {
                    "Significant decreasing hazard across ordered stages"
                }
            )
        }
        
        old_trend <- .calc(old_stage)
        new_trend <- .calc(new_stage)

        # The renderer also looks for a `comparison` element, which was never produced,
        # so the "Overall Comparison" row never appeared.
        comparison <- local({
            op <- old_trend$p_value; np <- new_trend$p_value
            oc <- old_trend$coefficient; nc <- new_trend$coefficient
            if (!is.finite(op) || !is.finite(np)) {
                list(interpretation = "Trend comparison not estimable")
            } else {
                list(
                    old_p = op, new_p = np,
                    old_coefficient = oc, new_coefficient = nc,
                    interpretation = if (abs(nc %||% 0) > abs(oc %||% 0)) {
                        "New staging shows the steeper monotonic gradient"
                    } else if (abs(nc %||% 0) < abs(oc %||% 0)) {
                        "Original staging shows the steeper monotonic gradient"
                    } else {
                        "Both systems show a comparable monotonic gradient"
                    }
                )
            }
        })

        list(old_trend = old_trend, new_trend = new_trend, comparison = comparison)
    }, error = function(e) list(old_trend=list(p_value=NA), new_trend=list(p_value=NA)))
}

stagemigration_calculatePseudoR2 <- function(old_cox, new_cox, data, options) {
    # Calculate various pseudo R-squared measures with robust error handling
    
    tryCatch({
        # Extract fitted model log-likelihoods
        if (is.null(old_cox$loglik) || length(old_cox$loglik) < 2) return(NULL)
        if (is.null(new_cox$loglik) || length(new_cox$loglik) < 2) return(NULL)

        ll_fitted_old <- old_cox$loglik[2]
        ll_fitted_new <- new_cox$loglik[2]
        
        # Null model log-likelihood (Approximation)
        # In full implementation we might fit a null model with covariates if multifactorial
        # For now, using the initial loglik is standard for Cox models in this context
        ll_null <- old_cox$loglik[1] 
        
        n <- nrow(data)
        p_old <- length(stats::coef(old_cox))
        p_new <- length(stats::coef(new_cox))
        
        safe_divide <- function(num, den) if (is.na(den) || den == 0) NA else num / den
        safe_exp <- function(x) if (is.na(x) || !is.finite(x)) NA else exp(x)
        
        # McFadden
        mcfadden_old <- if (ll_null != 0) 1 - safe_divide(ll_fitted_old, ll_null) else NA
        mcfadden_new <- if (ll_null != 0) 1 - safe_divide(ll_fitted_new, ll_null) else NA
        mcfadden_imp <- mcfadden_new - mcfadden_old
        
        # Cox-Snell
        cs_old <- if (n > 0) 1 - safe_exp((ll_null - ll_fitted_old) * 2 / n) else NA
        cs_new <- if (n > 0) 1 - safe_exp((ll_null - ll_fitted_new) * 2 / n) else NA
        cs_imp <- cs_new - cs_old
        
        # Nagelkerke
        max_exp <- safe_exp(ll_null * 2 / n)
        nk_max <- if (!is.na(max_exp)) 1 - max_exp else NA
        
        nk_old <- if (!is.na(nk_max) && nk_max > 0) safe_divide(cs_old, nk_max) else NA
        nk_new <- if (!is.na(nk_max) && nk_max > 0) safe_divide(cs_new, nk_max) else NA
        nk_imp <- nk_new - nk_old
        
        # Adjusted McFadden
        adj_mcfadden_old <- if (ll_null != 0) 1 - safe_divide((ll_fitted_old - p_old), ll_null) else NA
        adj_mcfadden_new <- if (ll_null != 0) 1 - safe_divide((ll_fitted_new - p_new), ll_null) else NA
        adj_mcfadden_imp <- adj_mcfadden_new - adj_mcfadden_old
        
        # Royston (Not implemented in helper yet, returning NA)
        royston_old <- NA
        royston_new <- NA
        royston_imp <- NA
        
        list(
            nagelkerke_old = nk_old,
            nagelkerke_new = nk_new,
            nagelkerke_improvement = nk_imp,
            mcfadden_old = mcfadden_old,
            mcfadden_new = mcfadden_new,
            mcfadden_improvement = mcfadden_imp,
            cox_snell_old = cs_old,
            cox_snell_new = cs_new,
            cox_snell_improvement = cs_imp,
            adj_mcfadden_old = adj_mcfadden_old,
            adj_mcfadden_new = adj_mcfadden_new,
            adj_mcfadden_improvement = adj_mcfadden_imp,
            royston_old = royston_old,
            royston_new = royston_new,
            royston_improvement = royston_imp
        )
    }, error = function(e) NULL)
}

# ---------------------------------------------------------------------------------------------
# Censoring-weighted (IPCW) NRI and IDI at a time horizon t.
#
# Status at t is known for patients with an event by t (cases) and for patients followed beyond t
# (controls); it is unknown for patients censored before t. The former estimators dropped those
# patients (NRI) or, for IDI, used ever-event status regardless of t, counting patients censored
# early as non-events. Both are biased when follow-up is incomplete. Each informative patient is
# instead weighted by the inverse probability of remaining uncensored, 1/G(T-) for cases and 1/G(t)
# for controls, with G the Kaplan-Meier estimate of the censoring distribution (Uno et al. 2007;
# Pencina et al. 2011). With no censoring before t every weight is 1 and the estimators reduce to
# the complete-case ones.
#
# Uncertainty: patients are resampled with their predicted risks held fixed and the weights are
# re-estimated in each resample; the CI is estimate +/- z * bootstrap SE. Uncertainty from fitting
# the Cox models is not propagated.
# ---------------------------------------------------------------------------------------------
stagemigration_ipcwWeights <- function(time, event, t) {
    cens <- survival::survfit(survival::Surv(time, 1 - event) ~ 1)
    G <- function(u, left) {
        idx <- findInterval(u, cens$time, left.open = left)
        out <- rep(1, length(u))
        out[idx > 0] <- cens$surv[idx[idx > 0]]
        out
    }
    case <- !is.na(time) & !is.na(event) & time <= t & event == 1
    control <- !is.na(time) & time > t
    w <- numeric(length(time))
    w[case] <- 1 / G(time[case], TRUE)
    w[control] <- 1 / G(t, FALSE)
    w[!is.finite(w)] <- 0
    list(w = w, case = case, control = control, g_t = G(t, FALSE))
}

stagemigration_ipcwNRI <- function(cat_old, cat_new, time, event, t) {
    wt <- stagemigration_ipcwWeights(time, event, t)
    ok <- !is.na(cat_old) & !is.na(cat_new)
    wc <- wt$w * (wt$case & ok)
    wn <- wt$w * (wt$control & ok)
    if (sum(wc) <= 0 || sum(wn) <= 0) {
        return(c(nri = NA_real_, nri_events = NA_real_, nri_nonevents = NA_real_))
    }
    up <- ok & cat_new > cat_old
    down <- ok & cat_new < cat_old
    up[is.na(up)] <- FALSE
    down[is.na(down)] <- FALSE
    nri_e <- (sum(wc[up]) - sum(wc[down])) / sum(wc)
    nri_ne <- (sum(wn[down]) - sum(wn[up])) / sum(wn)
    c(nri = nri_e + nri_ne, nri_events = nri_e, nri_nonevents = nri_ne)
}

stagemigration_ipcwIDI <- function(p_old, p_new, time, event, t) {
    wt <- stagemigration_ipcwWeights(time, event, t)
    ok <- is.finite(p_old) & is.finite(p_new)
    wc <- wt$w * (wt$case & ok)
    wn <- wt$w * (wt$control & ok)
    if (sum(wc) <= 0 || sum(wn) <= 0) {
        return(c(idi = NA_real_, old_events = NA_real_, old_nonevents = NA_real_, new_events = NA_real_, new_nonevents = NA_real_))
    }
    wmean <- function(p, ww) sum(ww[ok] * p[ok]) / sum(ww[ok])
    oe <- wmean(p_old, wc)
    on <- wmean(p_old, wn)
    ne <- wmean(p_new, wc)
    nn <- wmean(p_new, wn)
    c(idi = (ne - nn) - (oe - on), old_events = oe, old_nonevents = on, new_events = ne, new_nonevents = nn)
}

stagemigration_ipcwReps <- function(options) {
    r <- suppressWarnings(as.integer(options$bootstrapReps))
    if (length(r) != 1 || is.na(r)) r <- 500L
    max(200L, min(2000L, r))
}

stagemigration_coxRiskAt <- function(model, t, newdata) {
    surv_fit <- survival::survfit(model)
    idx <- findInterval(t, surv_fit$time)
    S0_t <- if (idx == 0) 1 else surv_fit$surv[idx]
    lp <- stats::predict(model, newdata = newdata, type = "lp")
    1 - (S0_t^exp(lp))
}

stagemigration_calculateNRI <- function(data, options, time_points = NULL, checkpoint_callback = NULL) {
    # Censoring-weighted Net Reclassification Improvement at each time horizon (see above).
    # Risk categories are tertiles of the POOLED predicted risks: one set of cut-points for both
    # systems. Separate tertiles per system (the former approach) put a third of each system's
    # patients in every category by construction, so "moving up" compared ranks, not risk.
    if (!isTRUE(options$calculateNRI)) return(NULL)

    old_stage <- options$oldStage
    new_stage <- options$newStage
    time_var <- options$survivalTime
    event_var <- "event_binary"

    if (is.null(time_points)) {
        time_points_str <- options$nriTimePoints
        if (!is.null(time_points_str)) {
            time_points <- as.numeric(unlist(strsplit(time_points_str, "\\s*,\\s*")))
            time_points <- time_points[!is.na(time_points)]
        }
    }
    if (length(time_points) == 0) time_points <- c(12, 24, 60)

    old_formula <- stats::as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", old_stage, "`", sep = ""))
    new_formula <- stats::as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", new_stage, "`", sep = ""))

    # The former tryCatch(..., error = function(e) return(list(error = ...))) returned from the
    # handler only, so a failed fit carried on with undefined models.
    fits <- tryCatch(
        list(old = survival::coxph(old_formula, data = data), new = survival::coxph(new_formula, data = data)),
        error = function(e) NULL
    )
    if (is.null(fits)) return(list(error = "Failed to fit Cox models for NRI"))

    time <- data[[time_var]]
    event <- data[[event_var]]
    n <- length(time)
    reps <- stagemigration_ipcwReps(options)
    z <- stagemigration_zcrit(options)
    nri_results <- list()

    for (time_point in time_points) {
        if (!is.null(checkpoint_callback)) checkpoint_callback()
        key <- paste0("t", time_point)
        if (time_point > max(time, na.rm = TRUE)) next

        risk_old <- stagemigration_coxRiskAt(fits$old, time_point, data)
        risk_new <- stagemigration_coxRiskAt(fits$new, time_point, data)
        cuts <- unique(stats::quantile(c(risk_old, risk_new), probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE, names = FALSE))
        if (length(cuts) < 3) {
            nri_results[[key]] <- list(time_point = time_point, error = "Predicted risks are too concentrated to form risk categories")
            next
        }
        cat_old <- cut(risk_old, breaks = cuts, include.lowest = TRUE, labels = FALSE)
        cat_new <- cut(risk_new, breaks = cuts, include.lowest = TRUE, labels = FALSE)

        wt <- stagemigration_ipcwWeights(time, event, time_point)
        ok <- !is.na(cat_old) & !is.na(cat_new)
        n_cases <- sum(wt$case & ok)
        n_controls <- sum(wt$control & ok)
        est <- stagemigration_ipcwNRI(cat_old, cat_new, time, event, time_point)
        if (n_cases == 0 || n_controls == 0 || !is.finite(est[["nri"]])) {
            nri_results[[key]] <- list(time_point = time_point, error = "Insufficient events/non-events")
            next
        }

        boot <- vapply(seq_len(reps), function(b) {
            if (!is.null(checkpoint_callback) && b %% 25 == 0) checkpoint_callback()
            i <- sample.int(n, n, replace = TRUE)
            stagemigration_ipcwNRI(cat_old[i], cat_new[i], time[i], event[i], time_point)[["nri"]]
        }, numeric(1))
        se <- stats::sd(boot, na.rm = TRUE)
        nri <- est[["nri"]]

        nri_results[[key]] <- list(
            time_point = time_point,
            nri_overall = nri,
            nri_events = est[["nri_events"]],
            nri_nonevents = est[["nri_nonevents"]],
            se = se,
            ci_lower = nri - z * se,
            ci_upper = nri + z * se,
            p_value = if (is.finite(se) && se > 0) 2 * stats::pnorm(-abs(nri / se)) else NA_real_,
            total_events = n_cases,
            total_patients = n_cases + n_controls,
            censored_before_t = sum(!wt$case & !wt$control),
            method = "IPCW"
        )
    }
    return(nri_results)
}

stagemigration_calculateIDI <- function(data, options, checkpoint_callback = NULL) {
    # Censoring-weighted Integrated Discrimination Improvement at the last NRI time point
    # (default 60 months), capped at the longest follow-up (see above).
    if (!isTRUE(options$calculateIDI)) return(NULL)

    old_stage <- options$oldStage
    new_stage <- options$newStage
    time_var <- options$survivalTime
    event_var <- "event_binary"

    old_formula <- stats::as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", old_stage, "`", sep = ""))
    new_formula <- stats::as.formula(paste("survival::Surv(`", time_var, "`, `", event_var, "`) ~ `", new_stage, "`", sep = ""))

    tryCatch({
        old_cox <- survival::coxph(old_formula, data = data)
        new_cox <- survival::coxph(new_formula, data = data)

        t_ref <- 60
        time_points_str <- options$nriTimePoints
        if (!is.null(time_points_str)) {
            tp <- suppressWarnings(as.numeric(unlist(strsplit(time_points_str, "\\s*,\\s*"))))
            tp <- tp[!is.na(tp)]
            if (length(tp) > 0) t_ref <- tp[length(tp)]
        }
        time <- data[[time_var]]
        event <- data[[event_var]]
        max_time <- max(time, na.rm = TRUE)
        if (t_ref > max_time) t_ref <- max_time

        old_prob <- stagemigration_coxRiskAt(old_cox, t_ref, data)
        new_prob <- stagemigration_coxRiskAt(new_cox, t_ref, data)

        wt <- stagemigration_ipcwWeights(time, event, t_ref)
        if (wt$g_t < 0.05) {
            return(list(error = sprintf("Fewer than 5%% of patients remain uncensored at %s months, too few for a censoring-weighted IDI.", format(t_ref))))
        }
        est <- stagemigration_ipcwIDI(old_prob, new_prob, time, event, t_ref)
        if (!is.finite(est[["idi"]])) return(list(error = "Insufficient events/non-events for IDI"))

        n <- length(time)
        reps <- stagemigration_ipcwReps(options)
        boot <- vapply(seq_len(reps), function(b) {
            if (!is.null(checkpoint_callback) && b %% 25 == 0) checkpoint_callback()
            i <- sample.int(n, n, replace = TRUE)
            stagemigration_ipcwIDI(old_prob[i], new_prob[i], time[i], event[i], t_ref)[["idi"]]
        }, numeric(1))
        se_idi <- stats::sd(boot, na.rm = TRUE)
        idi <- est[["idi"]]
        z <- stagemigration_zcrit(options)
        ok <- is.finite(old_prob) & is.finite(new_prob)

        list(
            idi = idi,
            idi_se = se_idi,
            idi_ci_lower = idi - z * se_idi,
            idi_ci_upper = idi + z * se_idi,
            idi_p_value = if (is.finite(se_idi) && se_idi > 0) 2 * stats::pnorm(-abs(idi / se_idi)) else NA_real_,
            old_discrimination_slope = est[["old_events"]] - est[["old_nonevents"]],
            new_discrimination_slope = est[["new_events"]] - est[["new_nonevents"]],
            old_prob_events = est[["old_events"]],
            old_prob_nonevents = est[["old_nonevents"]],
            new_prob_events = est[["new_events"]],
            new_prob_nonevents = est[["new_nonevents"]],
            n_events = sum(wt$case & ok),
            n_non_events = sum(wt$control & ok),
            censored_before_t = sum(!wt$case & !wt$control),
            time_point = t_ref,
            method = "IPCW",
            idi_bootstrap = NULL
        )
    }, error = function(e) list(error = conditionMessage(e)))
}
