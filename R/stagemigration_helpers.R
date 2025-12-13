#' Advanced Metrics Calculation for Stage Migration
#' @importFrom survival coxph Surv concordance survfit
#' @importFrom stats cor sd quantile complete.cases pnorm pchisq var approx AIC BIC
#' @importFrom Hmisc rcorrp.cens
#' @keywords internal

stagemigration_calculateAdvancedMetrics <- function(data, options, checkpoint_callback = NULL) {
    # Advanced discrimination and calibration metrics with comprehensive error handling
    
    old_stage <- options$oldStage
    new_stage <- options$newStage
    time_var <- options$survivalTime
    event_var <- "event_binary"

    # Fit Cox models
    old_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", old_stage))
    new_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", new_stage))

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

        # --- CORRECTION FOR INDEPENDENCE ASSUMPTION ---
        
        old_lp <- predict(old_cox, type = "lp")
        new_lp <- predict(new_cox, type = "lp")
        surv_obj <- survival::Surv(data[[time_var]], data[[event_var]])
        
        # Default values
        diff_se <- NA
        p_value <- NA
        
        # Try to use Hmisc::rcorrp.cens
        hmisc_result <- tryCatch({
            res <- Hmisc::rcorrp.cens(old_lp, new_lp, surv_obj)
            NULL 
        }, error = function(e) {
            NULL
        })
        
        
        # Manual Correlation Correction (Improved)
        valid_idx <- complete.cases(old_lp, new_lp)
        if (sum(valid_idx) > 10) {
            r_correlation <- cor(old_lp[valid_idx], new_lp[valid_idx], method = "spearman")
        } else {
            r_correlation <- 0
        }
        r_correlation <- max(-1, min(1, if(is.na(r_correlation)) 0 else r_correlation))

        if (old_var >= 0 && new_var >= 0) {
            old_se_val <- sqrt(old_var)
            new_se_val <- sqrt(new_var)
            covariance_term <- 2 * r_correlation * old_se_val * new_se_val
            diff_var <- old_var + new_var - covariance_term
            diff_var <- max(0, diff_var)
            diff_se <- sqrt(diff_var)
            z_stat <- if (diff_se > 0) c_improvement / diff_se else NA
            p_value <- if (!is.na(z_stat)) 2 * (1 - pnorm(abs(z_stat))) else NA
        }

        use_bootstrap <- options$analysisType %in% c("comprehensive", "publication") && options$performBootstrap
        c_bootstrap <- NULL
        c_improvement_ci_lower <- NA
        c_improvement_ci_upper <- NA

        if (use_bootstrap) {
            c_bootstrap <- stagemigration_compareBootstrapCIndex(
                data, old_stage, new_stage, time_var, event_var,
                n_boot = options$bootstrapReps %||% 200,
                checkpoint_callback = checkpoint_callback
            )
            p_value <- c_bootstrap$p_value
            diff_se <- c_bootstrap$se
            c_improvement_ci_lower <- c_bootstrap$ci_lower
            c_improvement_ci_upper <- c_bootstrap$ci_upper
        } else { 
             if (!is.na(c_improvement) && !is.na(diff_se)) {
                c_improvement_ci_lower <- c_improvement - 1.96 * diff_se
                c_improvement_ci_upper <- c_improvement + 1.96 * diff_se
            }
        }

        aic_old <- AIC(old_cox)
        aic_new <- AIC(new_cox)
        aic_improvement <- aic_old - aic_new
        bic_old <- BIC(old_cox)
        bic_new <- BIC(new_cox)
        bic_improvement <- bic_old - bic_new

        lr_test <- tryCatch({
            combined_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~ ",
                                                old_stage, " + ", new_stage))
            combined_cox <- survival::coxph(combined_formula, data = data)
            lr_new <- 2 * (combined_cox$loglik[2] - new_cox$loglik[2])
            df_new <- length(coef(combined_cox)) - length(coef(new_cox))
            p_new <- pchisq(lr_new, df_new, lower.tail = FALSE)
            lr_old <- 2 * (combined_cox$loglik[2] - old_cox$loglik[2])
            list(lr_stat = lr_new - lr_old, df = df_new, p_value = p_new)
        }, error = function(e) {
            list(lr_stat = NA, df = NA, p_value = NA)
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
        message("ERROR in calculateAdvancedMetrics: ", e$message)
        return(list(error = e$message))
    })
}

stagemigration_compareBootstrapCIndex <- function(data, old_stage, new_stage, time_var, event_var, n_boot = 200, checkpoint_callback = NULL) {
    # Bootstrap comparison of C-indices for correlated data
    tryCatch({
        n <- nrow(data)
        c_diffs <- numeric(n_boot)

        old_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", old_stage))
        new_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", new_stage))

        # Calculate original difference
        old_cox_orig <- survival::coxph(old_formula, data = data)
        new_cox_orig <- survival::coxph(new_formula, data = data)
        c_diff_orig <- survival::concordance(new_cox_orig)$concordance - survival::concordance(old_cox_orig)$concordance

        for (i in 1:n_boot) {
            if (!is.null(checkpoint_callback) && i %% 50 == 1) checkpoint_callback()
            
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
        p_value <- 2 * min(mean(c_diffs <= 0), mean(c_diffs >= 0))
        
        ci_lower <- quantile(c_diffs, 0.025, na.rm = TRUE)
        ci_upper <- quantile(c_diffs, 0.975, na.rm = TRUE)

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
            list(stat = s$waldtest["test"], p_value = s$waldtest["pvalue"])
        }
        
        old_trend <- .calc(old_stage)
        new_trend <- .calc(new_stage)
        
        list(old_trend = old_trend, new_trend = new_trend)
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
        p_old <- length(coef(old_cox))
        p_new <- length(coef(new_cox))
        
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

stagemigration_calculateNRI <- function(data, options, time_points = NULL, checkpoint_callback = NULL) {
    # Net Reclassification Improvement
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

    old_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", old_stage))
    new_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", new_stage))

    tryCatch({
        cox_original <- survival::coxph(old_formula, data = data)
        cox_modified <- survival::coxph(new_formula, data = data)
    }, error = function(e) return(list(error = "Failed to fit Cox models for NRI")))

    nri_results <- list()

    get_risk_at_t <- function(model, t, newdata) {
        surv_fit <- survival::survfit(model)
        idx <- findInterval(t, surv_fit$time)
        S0_t <- if (idx == 0) 1 else surv_fit$surv[idx]
        lp <- predict(model, newdata = newdata, type = "lp")
        1 - (S0_t ^ exp(lp))
    }

    for (time_point in time_points) {
        if (!is.null(checkpoint_callback)) checkpoint_callback()
        
        if (time_point > max(data[[time_var]], na.rm = TRUE)) next

        risk_original <- get_risk_at_t(cox_original, time_point, data)
        risk_modified <- get_risk_at_t(cox_modified, time_point, data)

        is_event <- data[[time_var]] <= time_point & data[[event_var]] == 1
        is_nonevent <- data[[time_var]] > time_point
        
        events_idx <- which(is_event)
        nonevents_idx <- which(is_nonevent)
        
        if (length(events_idx) == 0 || length(nonevents_idx) == 0) {
            nri_results[[paste0("t", time_point)]] <- list(time_point = time_point, error = "Insufficient events/non-events")
            next
        }

        risk_cuts_original <- quantile(risk_original, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
        risk_cuts_modified <- quantile(risk_modified, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

        if(length(unique(risk_cuts_original)) < 4) risk_cuts_original <- unique(quantile(risk_original, probs = seq(0, 1, length.out = length(unique(risk_original))+1), na.rm = TRUE))
        if(length(unique(risk_cuts_modified)) < 4) risk_cuts_modified <- unique(quantile(risk_modified, probs = seq(0, 1, length.out = length(unique(risk_modified))+1), na.rm = TRUE))
        if(length(risk_cuts_original) < 2) risk_cuts_original <- c(0, 0.5, 1)
        if(length(risk_cuts_modified) < 2) risk_cuts_modified <- c(0, 0.5, 1)

        risk_cat_original <- cut(risk_original, breaks = risk_cuts_original, include.lowest = TRUE, labels = FALSE)
        risk_cat_modified <- cut(risk_modified, breaks = risk_cuts_modified, include.lowest = TRUE, labels = FALSE)

        risk_cat_orig_events <- risk_cat_original[events_idx]
        risk_cat_mod_events <- risk_cat_modified[events_idx]
        
        improved_events <- sum(risk_cat_mod_events > risk_cat_orig_events, na.rm = TRUE)
        worsened_events <- sum(risk_cat_mod_events < risk_cat_orig_events, na.rm = TRUE)
        n_events_valid <- length(risk_cat_orig_events)
        
        nri_events <- (improved_events - worsened_events) / n_events_valid

        risk_cat_orig_nonevents <- risk_cat_original[nonevents_idx]
        risk_cat_mod_nonevents <- risk_cat_modified[nonevents_idx]
        
        improved_nonevents <- sum(risk_cat_mod_nonevents < risk_cat_orig_nonevents, na.rm = TRUE)
        worsened_nonevents <- sum(risk_cat_mod_nonevents > risk_cat_orig_nonevents, na.rm = TRUE)
        n_nonevents_valid <- length(risk_cat_orig_nonevents)
        
        nri_non_events <- (improved_nonevents - worsened_nonevents) / n_nonevents_valid

        nri_total <- nri_events + nri_non_events

        var_events <- (improved_events + worsened_events) / (n_events_valid^2) - (nri_events^2 / n_events_valid)
        var_nonevents <- (improved_nonevents + worsened_nonevents) / (n_nonevents_valid^2) - (nri_non_events^2 / n_nonevents_valid)
        
        se_nri <- sqrt(max(0, var_events) + max(0, var_nonevents))
        
        ci_lower <- nri_total - 1.96 * se_nri
        ci_upper <- nri_total + 1.96 * se_nri
        z_score <- if (se_nri > 0) nri_total / se_nri else 0
        p_value <- if (se_nri > 0) 2 * (1 - pnorm(abs(z_score))) else 1

        nri_results[[paste0("t", time_point)]] <- list(
            time_point = time_point,
            nri_overall = nri_total,
            nri_events = nri_events,
            nri_nonevents = nri_non_events,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            p_value = p_value,
            total_events = n_events_valid,
            total_patients = n_events_valid + n_nonevents_valid
        )
    }
    return(nri_results)
}

stagemigration_calculateIDI <- function(data, options, checkpoint_callback = NULL) {
    # Integrated Discrimination Improvement
    if (!isTRUE(options$calculateIDI)) return(NULL)

    old_stage <- options$oldStage
    new_stage <- options$newStage
    time_var <- options$survivalTime
    event_var <- "event_binary"

    old_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", old_stage))
    new_formula <- as.formula(paste("survival::Surv(", time_var, ",", event_var, ") ~", new_stage))

    tryCatch({
        old_cox <- survival::coxph(old_formula, data = data)
        new_cox <- survival::coxph(new_formula, data = data)
        
        old_lp <- predict(old_cox, type = "lp")
        new_lp <- predict(new_cox, type = "lp")
        
        old_prob <- exp(old_lp) / (1 + exp(old_lp))
        new_prob <- exp(new_lp) / (1 + exp(new_lp))
        
        events <- data[[event_var]]
        
        old_disc_events <- mean(old_prob[events == 1], na.rm = TRUE)
        old_disc_nonevents <- mean(old_prob[events == 0], na.rm = TRUE)
        old_disc_slope <- old_disc_events - old_disc_nonevents
        
        new_disc_events <- mean(new_prob[events == 1], na.rm = TRUE)
        new_disc_nonevents <- mean(new_prob[events == 0], na.rm = TRUE)
        new_disc_slope <- new_disc_events - new_disc_nonevents
        
        idi <- new_disc_slope - old_disc_slope
        
        n_events <- sum(events == 1, na.rm = TRUE)
        n_nonevents <- sum(events == 0, na.rm = TRUE)
        
        var_old_events <- if (n_events > 1) var(old_prob[events == 1], na.rm = TRUE) / n_events else 0
        var_old_nonevents <- if (n_nonevents > 1) var(old_prob[events == 0], na.rm = TRUE) / n_nonevents else 0
        var_new_events <- if (n_events > 1) var(new_prob[events == 1], na.rm = TRUE) / n_events else 0
        var_new_nonevents <- if (n_nonevents > 1) var(new_prob[events == 0], na.rm = TRUE) / n_nonevents else 0
        
        se_idi <- sqrt((var_new_events + var_new_nonevents) + (var_old_events + var_old_nonevents))
        
        ci_lower <- idi - 1.96 * se_idi
        ci_upper <- idi + 1.96 * se_idi
        
        z_score <- if (se_idi > 0) idi / se_idi else 0
        p_value <- if (se_idi > 0) 2 * (1 - pnorm(abs(z_score))) else 1
        
        idi_bootstrap <- NULL
        if (isTRUE(options$performBootstrap) && (n_events + n_nonevents) > 50) {
             # We might need to expose .bootstrapIDI logic too or just inline it here or assume it's available? 
             # It was a private method. I will add a simplified version here.
             idi_bootstrap <- stagemigration_bootstrapIDI(data, old_formula, new_formula, options$bootstrapReps, checkpoint_callback)
        }
        
        list(
            idi = idi,
            idi_se = se_idi,
            idi_ci_lower = ci_lower,
            idi_ci_upper = ci_upper,
            idi_p_value = p_value,
            old_discrimination_slope = old_disc_slope,
            new_discrimination_slope = new_disc_slope,
            old_prob_events = old_disc_events,
            old_prob_nonevents = old_disc_nonevents,
            new_prob_events = new_disc_events,
            new_prob_nonevents = new_disc_nonevents,
            n_events = n_events,
            n_non_events = n_nonevents,
            idi_bootstrap = idi_bootstrap
        )
    }, error = function(e) list(error = e$message))
}

stagemigration_bootstrapIDI <- function(data, old_formula, new_formula, reps = 200, checkpoint_callback = NULL) {
    # Bootstrap for IDI
    if (!requireNamespace("boot", quietly=TRUE)) return(NULL)
    
    if (is.null(reps)) reps <- 200
    if (reps > 500) reps <- 500 # Limit
    
    boot_fn <- function(data, indices) {
        boot_data <- data[indices, ]
        tryCatch({
            old_c <- survival::coxph(old_formula, data = boot_data)
            new_c <- survival::coxph(new_formula, data = boot_data)
            
            old_p <- exp(predict(old_c, type="lp")) / (1 + exp(predict(old_c, type="lp")))
            new_p <- exp(predict(new_c, type="lp")) / (1 + exp(predict(new_c, type="lp")))
            
            ev <- boot_data[["event_binary"]]
            old_slope <- mean(old_p[ev==1], na.rm=TRUE) - mean(old_p[ev==0], na.rm=TRUE)
            new_slope <- mean(new_p[ev==1], na.rm=TRUE) - mean(new_p[ev==0], na.rm=TRUE)
            new_slope - old_slope
        }, error = function(e) NA)
    }
    
    tryCatch({
        boot_res <- boot::boot(data = data, statistic = boot_fn, R = reps)
        boot::boot.ci(boot_res, type = "perc")
    }, error = function(e) NULL)
}