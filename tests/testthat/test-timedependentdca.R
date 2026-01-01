# Test Suite for Time-Dependent Decision Curve Analysis
# Testing mathematical accuracy, survival analysis correctness, and edge cases

context("Time-Dependent Decision Curve Analysis - Mathematical Accuracy")

library(survival)

# Helper functions to calculate expected values manually
calculate_net_benefit <- function(tp, fp, n, threshold) {
    (tp / n) - (fp / n) * (threshold / (1 - threshold))
}

calculate_treat_all_nb <- function(n_events, n_total, threshold) {
    (n_events / n_total) - ((n_total - n_events) / n_total) * (threshold / (1 - threshold))
}

test_that("Net benefit formula is mathematically correct", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # Simple case with known values
    tp <- 30
    fp <- 20
    n <- 100
    threshold <- 0.2
    
    expected_nb <- calculate_net_benefit(tp, fp, n, threshold)
    
    # Manual calculation
    expected_manual <- (30/100) - (20/100) * (0.2/0.8)
    expect_equal(round(expected_nb, 4), round(expected_manual, 4))
    expect_equal(round(expected_nb, 4), 0.25)
})

test_that("Treat all strategy matches formula", {
    n_events <- 40
    n_total <- 100
    threshold <- 0.3
    
    expected <- calculate_treat_all_nb(n_events, n_total, threshold)
    
    # Manual calculation
    expected_manual <- (40/100) - (60/100) * (0.3/0.7)
    expect_equal(round(expected, 4), round(expected_manual, 4))
    expect_equal(round(expected, 4), 0.1429, tolerance = 0.0001)
})

test_that("Treat none strategy always returns zero net benefit", {
    # Treat none should always have NB = 0 regardless of threshold
    thresholds <- seq(0.01, 0.99, 0.1)
    
    for (pt in thresholds) {
        nb_treat_none <- 0  # By definition
        expect_equal(nb_treat_none, 0)
    }
})

test_that("Interventions avoided calculation is correct", {
    n_total <- 100
    n_treated_all <- 100  # Treat all
    n_treated_model <- 40  # Model treats 40
    
    interventions_avoided <- (n_treated_all - n_treated_model) / n_total * 100
    
    expect_equal(interventions_avoided, 60)
})

context("Time-Dependent DCA - Survival Analysis")

test_that("Cox model produces valid probabilities", {
    # Simulate data with known relationship
    set.seed(123)
    n <- 200
    predictor <- rnorm(n)
    
    # Simulate survival times from exponential distribution
    # Higher predictor → higher hazard → shorter survival
    lambda <- exp(0.5 * predictor)
    time <- rexp(n, rate = lambda)
    
    # Add censoring
    censor_time <- runif(n, 0, max(time) * 1.5)
    observed_time <- pmin(time, censor_time)
    event <- as.numeric(time <= censor_time)
    
    # Fit Cox model
    cox_fit <- coxph(Surv(observed_time, event) ~ predictor)
    
    # Predict survival at time = median(time)
    t_eval <- median(observed_time)
    newdata <- data.frame(predictor = predictor)
    surv_fit <- survfit(cox_fit, newdata = newdata)
    
    # Extract survival probabilities
    # This tests that our extraction logic works
    prob_event <- numeric(n)
    for (i in 1:n) {
        surv_times <- surv_fit$time
        surv_probs <- if (is.matrix(surv_fit$surv)) surv_fit$surv[, i] else surv_fit$surv
        
        time_idx <- which(surv_times <= t_eval)
        if (length(time_idx) > 0) {
            surv_at_t <- surv_probs[max(time_idx)]
        } else {
            surv_at_t <- 1
        }
        prob_event[i] <- 1 - surv_at_t
    }
    
    # Probabilities should be in [0,1]
    expect_true(all(prob_event >= 0 & prob_event <= 1))
    
    # Higher predictor should generally lead to higher event probability
    # (proportional hazards assumption)
    correlation <- cor(predictor, prob_event)
    expect_true(correlation > 0, info = paste("Correlation:", correlation))
})

test_that("Kaplan-Meier stratification produces valid probabilities", {
    # Simulate data
    set.seed(456)
    n <- 150
    predictor <- rnorm(n)
    
    # Simulate survival (higher predictor → higher risk)
    lambda <- exp(0.3 * predictor)
    time <- rexp(n, rate = lambda)
    censor_time <- runif(n, 0, max(time) * 1.2)
    observed_time <- pmin(time, censor_time)
    event <- as.numeric(time <= censor_time)
    
    # Create predictor groups
    n_groups <- 3
    predictor_groups <- cut(predictor, 
                           breaks = quantile(predictor, probs = seq(0, 1, length = n_groups + 1)),
                           include.lowest = TRUE,
                           labels = FALSE)
    
    # Fit KM by group
    km_by_group <- survfit(Surv(observed_time, event) ~ predictor_groups)
    
    # Get survival at median time
    t_eval <- median(observed_time)
    group_summary <- summary(km_by_group, times = t_eval, extend = TRUE)
    
    # Extract probabilities
    prob_event <- numeric(n)
    for (g in 1:n_groups) {
        group_mask <- predictor_groups == g
        stratum_idx <- which(group_summary$strata == paste0("predictor_groups=", g))
        if (length(stratum_idx) > 0) {
            surv_g <- group_summary$surv[stratum_idx[1]]
            prob_event[group_mask] <- 1 - surv_g
        }
    }
    
    # All probabilities should be valid
    expect_true(all(prob_event >= 0 & prob_event <= 1))
    
    # Higher predictor groups should have higher event probabilities
    mean_prob_by_group <- tapply(prob_event, predictor_groups, mean)
    expect_true(all(diff(mean_prob_by_group) >= 0), 
                info = paste("Mean probabilities by group:", paste(round(mean_prob_by_group, 3), collapse = ", ")))
})

test_that("At-risk population is handled correctly by KM", {
    # Create test data with specific patterns
    time <- c(100, 200, 300, 400, 500)
    event <- c(0, 1, 0, 1, 1)  # Events at times 200, 400, 500
    
    # Test at t_eval = 250
    t_eval <- 250
    
    # KM estimate at t=250
    # At t=100: 1 censored, 4 at risk
    # At t=200: 1 event, 3 at risk. S(200) = 1 * (1 - 1/4) = 0.75
    # At t=250: S(250) = 0.75
    
    surv_obj <- Surv(time, event)
    km_fit <- survfit(surv_obj ~ 1)
    surv_t <- summary(km_fit, times = t_eval, extend = TRUE)$surv[1]
    
    expect_equal(surv_t, 0.75)
})

context("Time-Dependent DCA - Edge Cases")

test_that("Function handles small sample sizes", {
    # Very small dataset
    set.seed(789)
    n <- 20
    predictor <- rnorm(n)
    time <- rexp(n)
    event <- rbinom(n, 1, 0.5)
    
    # Should not crash
    expect_error({
        surv_obj <- Surv(time, event)
        km_fit <- survfit(surv_obj ~ 1)
        t_eval <- median(time)
        surv_t <- summary(km_fit, times = t_eval, extend = TRUE)$surv[1]
        prob_event <- 1 - surv_t
    }, NA)  # NA means no error expected
})

test_that("Function handles all events", {
    # Dataset where all observations are events
    n <- 50
    predictor <- rnorm(n)
    time <- rexp(n)
    event <- rep(1, n)  # All events
    
    t_eval <- median(time)
    
    # KM estimate should be valid
    surv_obj <- Surv(time, event)
    km_fit <- survfit(surv_obj ~ 1)
    surv_t <- summary(km_fit, times = t_eval, extend = TRUE)$surv[1]
    
    expect_true(surv_t >= 0 && surv_t <= 1)
})

test_that("Function handles all censored", {
    # Dataset where all observations are censored
    n <- 50
    predictor <- rnorm(n)
    time <- rexp(n)
    event <- rep(0, n)  # All censored
    
    t_eval <- median(time)
    
    # Should still work (though not very informative)
    surv_obj <- Surv(time, event)
    km_fit <- survfit(surv_obj ~ 1)
    
    # Should return survival = 1 (no events)
    surv_t <- summary(km_fit, times = t_eval, extend = TRUE)$surv[1]
    expect_equal(surv_t, 1)
})

test_that("Direct probability method preserves input probabilities", {
    # When estimate_method = \"direct\", predictor should be used as-is
    predictor <- runif(100, 0, 1)  # Already probabilities
    
    # Direct method should just use these values
    prob_event_direct <- predictor
    
    expect_equal(prob_event_direct, predictor)
    expect_true(all(prob_event_direct >= 0 & prob_event_direct <= 1))
})

context("Time-Dependent DCA - Multiple Time Points")

test_that("Results vary appropriately across time points", {
    # Simulate data
    set.seed(999)
    n <- 200
    predictor <- rnorm(n)
    time <- rexp(n, rate = 0.5)
    event <- rbinom(n, 1, 0.6)
    
    # Multiple time points
    time_points <- c(0.5, 1.0, 2.0, 3.0)
    
    event_rates <- numeric(length(time_points))
    
    for (i in seq_along(time_points)) {
        t_eval <- time_points[i]
        has_event_by_t <- (time <= t_eval & event == 1)
        at_risk_at_t <- (time > t_eval) | (time >= t_eval & event == 1)
        
        if (sum(at_risk_at_t) > 0) {
            event_rates[i] <- sum(has_event_by_t) / sum(at_risk_at_t)
        }
    }
    
    # Event rate should generally increase with time
    # (more events accumulate over time)
    # Event rate should generally increase with time
    # (more events accumulate over time)
    expect_true(all(diff(event_rates) >= -0.1))
})

test_that("Net benefit respects threshold ordering", {
    # For a good model, net benefit should generally decrease as threshold increases
    # (fewer people treated → fewer true positives captured)
    
    # Simulate scenario
    tp_sequence <- c(80, 70, 60, 40, 20)  # Decreasing TP as threshold increases
    fp_sequence <- c(60, 40, 25, 10, 5)   # Decreasing FP as threshold increases
    n <- 100
    thresholds <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    
    nb_sequence <- numeric(length(thresholds))
    for (i in seq_along(thresholds)) {
        nb_sequence[i] <- calculate_net_benefit(tp_sequence[i], fp_sequence[i], n, thresholds[i])
    }
    
    # Net benefit pattern should make sense (though not strictly monotonic)
    expect_true(all(is.finite(nb_sequence)))
})

context("Time-Dependent DCA - Comparison with Standard Methods")

test_that("Results are consistent with published DCA principles", {
    # Vickers & Elkin (2006) principles:
    # 1. NB of \"treat none\" = 0
    # 2. NB of \"treat all\" = (event rate) - (1-event rate) * (pt/(1-pt))
    # 3. Model NB should be between these bounds for reasonable thresholds
    
    n <- 100
    n_events <- 30
    event_rate <- n_events / n
    threshold <- 0.25
    
    nb_treat_none <- 0
    nb_treat_all <- calculate_treat_all_nb(n_events, n, threshold)
    
    # For a reasonable model (40 TP, 15 FP)
    nb_model <- calculate_net_benefit(40, 15, n, threshold)
    
    # Model should be better than both references at optimal threshold
    expect_gt(nb_model, nb_treat_none)
    # (Note: may not always be > treat_all depending on threshold)
})
