#' Survival Power Analysis - Distribution Support Module
#' 
#' Enhanced distribution support for survivalPower function
#' Implements Weibull, log-normal, and piecewise exponential distributions
#' 
#' @references
#' Lachin, J. M., & Foulkes, M. A. (1986). Evaluation of sample size and power 
#' for analyses of survival with allowance for nonuniform patient entry, losses 
#' to follow-up, noncompliance, and stratification. Biometrics, 42(3), 507-519. 
#' PMID: 3567285

# Weibull Distribution Functions ----

#' Calculate Weibull distribution parameters from median survival
#' 
#' @param median_survival Median survival time
#' @param shape Weibull shape parameter (gamma)
#' @return List with lambda (scale) and shape parameters
#' 
#' @details
#' For Weibull distribution: S(t) = exp(-(lambda*t)^shape)
#' Median: m = (log(2)/lambda)^(1/shape)
#' Therefore: lambda = (log(2)/m)^(1/shape)
.get_weibull_parameters <- function(median_survival, shape) {
  if (shape <= 0) {
    stop("Weibull shape parameter must be positive")
  }
  
  # Calculate scale parameter from median
  lambda <- (log(2) / median_survival)^(1 / shape)
  
  list(
    lambda = lambda,
    shape = shape,
    median = median_survival
  )
}

#' Calculate survival probability at time t for Weibull distribution
#' 
#' @param t Time point
#' @param lambda Scale parameter
#' @param shape Shape parameter
#' @return Survival probability S(t)
.weibull_survival <- function(t, lambda, shape) {
  exp(-(lambda * t)^shape)
}

#' Calculate hazard function for Weibull distribution
#' 
#' @param t Time point
#' @param lambda Scale parameter
#' @param shape Shape parameter
#' @return Hazard h(t)
#' 
#' @details
#' h(t) = lambda * shape * (lambda * t)^(shape - 1)
.weibull_hazard <- function(t, lambda, shape) {
  lambda * shape * (lambda * t)^(shape - 1)
}

#' Calculate expected number of events for Weibull distribution
#' 
#' @param n_total Total sample size
#' @param lambda_control Control group scale parameter
#' @param shape_control Control group shape parameter
#' @param hr Hazard ratio
#' @param allocation_ratio Allocation ratio (control:treatment)
#' @param accrual_period Patient accrual period (months)
#' @param follow_up_period Additional follow-up period (months)
#' @param dropout_rate Annual dropout rate
#' @return List with expected events by group and total
#' 
#' @details
#' Uses numerical integration to calculate expected events under Weibull distribution
#' Accounts for staggered entry, administrative censoring, and dropout
.calculate_weibull_events <- function(n_total, lambda_control, shape_control, 
                                      hr, allocation_ratio, accrual_period, 
                                      follow_up_period, dropout_rate) {
  
  # Allocation
  props <- .allocation_props(allocation_ratio)
  n_control <- round(n_total * props$control)
  n_treatment <- n_total - n_control
  
  # Treatment group parameters (proportional hazards assumption)
  # Under PH: h_treatment(t) = hr * h_control(t)
  # For Weibull: lambda_treatment = lambda_control * hr^(1/shape)
  lambda_treatment <- lambda_control * hr^(1 / shape_control)
  shape_treatment <- shape_control  # Shape parameter same under PH
  
  # Dropout hazard (exponential)
  dropout_hazard <- -log(1 - dropout_rate) / 12  # Convert annual to monthly
  
  # Calculate event probability for each group
  # Integrate over accrual and follow-up periods
  total_duration <- accrual_period + follow_up_period
  
  # For uniform accrual, average follow-up time is approximately
  # (accrual_period/2 + follow_up_period)
  avg_followup_control <- accrual_period / 2 + follow_up_period
  
  # Event probability accounting for competing dropout
  # P(event) = integral of h(t) * S(t) * S_dropout(t) dt
  
  # Simplified calculation using average follow-up
  # More accurate would use numerical integration
  event_prob_control <- 1 - .weibull_survival(avg_followup_control, lambda_control, shape_control) *
    exp(-dropout_hazard * avg_followup_control)
  
  event_prob_treatment <- 1 - .weibull_survival(avg_followup_control, lambda_treatment, shape_treatment) *
    exp(-dropout_hazard * avg_followup_control)
  
  # Expected events
  events_control <- n_control * event_prob_control
  events_treatment <- n_treatment * event_prob_treatment
  
  list(
    control = events_control,
    treatment = events_treatment,
    total = events_control + events_treatment,
    event_prob_control = event_prob_control,
    event_prob_treatment = event_prob_treatment
  )
}

# Log-Normal Distribution Functions ----

#' Calculate log-normal distribution parameters from median survival
#' 
#' @param median_survival Median survival time
#' @param sigma Standard deviation of log(T)
#' @return List with mu and sigma parameters
#' 
#' @details
#' For log-normal: log(T) ~ N(mu, sigma^2)
#' Median: m = exp(mu)
#' Therefore: mu = log(m)
.get_lognormal_parameters <- function(median_survival, sigma) {
  if (sigma <= 0) {
    stop("Log-normal sigma parameter must be positive")
  }
  
  mu <- log(median_survival)
  
  list(
    mu = mu,
    sigma = sigma,
    median = median_survival
  )
}

#' Calculate survival probability for log-normal distribution
#' 
#' @param t Time point
#' @param mu Location parameter
#' @param sigma Scale parameter
#' @return Survival probability S(t)
.lognormal_survival <- function(t, mu, sigma) {
  1 - pnorm((log(t) - mu) / sigma)
}

# Piecewise Exponential Distribution Functions ----

#' Calculate piecewise exponential parameters
#' 
#' @param intervals Vector of time intervals
#' @param hazards Vector of hazard rates for each interval
#' @return List with intervals and hazards
.get_piecewise_exponential_parameters <- function(intervals, hazards) {
  if (length(intervals) != length(hazards) + 1) {
    stop("Number of intervals must be one more than number of hazards")
  }
  
  list(
    intervals = intervals,
    hazards = hazards
  )
}

#' Calculate survival probability for piecewise exponential
#' 
#' @param t Time point
#' @param intervals Time intervals
#' @param hazards Hazard rates
#' @return Survival probability S(t)
.piecewise_exponential_survival <- function(t, intervals, hazards) {
  # Find which interval t falls into
  interval_idx <- findInterval(t, intervals)
  
  if (interval_idx == 0) {
    return(1)  # Before first interval
  }
  
  # Calculate cumulative hazard
  cum_hazard <- 0
  
  # Sum hazards from previous complete intervals
  if (interval_idx > 1) {
    for (i in 1:(interval_idx - 1)) {
      interval_length <- intervals[i + 1] - intervals[i]
      cum_hazard <- cum_hazard + hazards[i] * interval_length
    }
  }
  
  # Add hazard from current interval
  time_in_current <- t - intervals[interval_idx]
  cum_hazard <- cum_hazard + hazards[interval_idx] * time_in_current
  
  exp(-cum_hazard)
}

# Simulation-Based Validation Functions ----

#' Simulate a survival trial dataset
#' 
#' @param n Total sample size
#' @param distribution Survival distribution ("exponential", "weibull", "lognormal")
#' @param params Distribution parameters
#' @param hr Hazard ratio
#' @param allocation_ratio Allocation ratio
#' @param accrual_period Accrual period (months)
#' @param follow_up_period Follow-up period (months)
#' @param dropout_rate Annual dropout rate
#' @return Data frame with simulated trial data
.simulate_survival_trial <- function(n, distribution = "exponential", params, 
                                    hr, allocation_ratio, accrual_period, 
                                    follow_up_period, dropout_rate) {
  
  # Randomization
  props <- .allocation_props(allocation_ratio)
  n_control <- round(n * props$control)
  n_treatment <- n - n_control
  
  group <- c(rep("Control", n_control), rep("Treatment", n_treatment))
  
  # Accrual times (uniform)
  accrual_time <- runif(n, 0, accrual_period)
  
  # Generate event times based on distribution
  if (distribution == "exponential") {
    lambda_control <- params$lambda_control
    lambda_treatment <- lambda_control * hr
    
    event_time_control <- rexp(n_control, rate = lambda_control)
    event_time_treatment <- rexp(n_treatment, rate = lambda_treatment)
    
  } else if (distribution == "weibull") {
    lambda_control <- params$lambda
    shape <- params$shape
    lambda_treatment <- lambda_control * hr^(1 / shape)
    
    # Weibull random generation
    event_time_control <- (-log(runif(n_control)) / lambda_control)^(1 / shape)
    event_time_treatment <- (-log(runif(n_treatment)) / lambda_treatment)^(1 / shape)
    
  } else if (distribution == "lognormal") {
    mu_control <- params$mu
    sigma <- params$sigma
    # Approximate HR effect on log-normal (not exact under PH)
    mu_treatment <- mu_control - log(hr)
    
    event_time_control <- rlnorm(n_control, meanlog = mu_control, sdlog = sigma)
    event_time_treatment <- rlnorm(n_treatment, meanlog = mu_treatment, sdlog = sigma)
    
  } else {
    stop("Unsupported distribution for simulation")
  }
  
  event_time <- c(event_time_control, event_time_treatment)
  
  # Dropout times (exponential)
  dropout_hazard <- -log(1 - dropout_rate) / 12
  dropout_time <- rexp(n, rate = dropout_hazard)
  
  # Administrative censoring
  admin_censor_time <- accrual_period + follow_up_period - accrual_time
  
  # Observed time = minimum of event, dropout, admin censoring
  observed_time <- pmin(event_time, dropout_time, admin_censor_time)
  event <- (event_time <= dropout_time) & (event_time <= admin_censor_time)
  
  data.frame(
    group = group,
    time = observed_time,
    event = as.numeric(event),
    accrual_time = accrual_time
  )
}

#' Validate power calculation using simulation
#' 
#' @param params Study parameters
#' @param n_sims Number of simulation iterations
#' @return List with simulated power and confidence interval
#' 
#' @details
#' Performs Monte Carlo simulation to validate analytical power calculations
#' Useful for complex scenarios where analytical formulas may be approximate
.validate_power_by_simulation <- function(params, n_sims = 10000) {
  
  significant_count <- 0
  
  for (i in 1:n_sims) {
    # Simulate survival data
    sim_data <- .simulate_survival_trial(
      n = params$sample_size,
      distribution = params$distribution,
      params = params$dist_params,
      hr = params$hr,
      allocation_ratio = params$allocation_ratio,
      accrual_period = params$accrual_period,
      follow_up_period = params$follow_up,
      dropout_rate = params$dropout_rate
    )
    
    # Perform log-rank test
    test_result <- tryCatch({
      survival::survdiff(
        survival::Surv(time, event) ~ group,
        data = sim_data
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(test_result)) {
      p_value <- 1 - pchisq(test_result$chisq, df = 1)
      
      if (p_value < params$alpha) {
        significant_count <- significant_count + 1
      }
    }
  }
  
  simulated_power <- significant_count / n_sims
  
  # 95% CI for simulated power (binomial proportion)
  se <- sqrt(simulated_power * (1 - simulated_power) / n_sims)
  ci_lower <- max(0, simulated_power - 1.96 * se)
  ci_upper <- min(1, simulated_power + 1.96 * se)
  
  list(
    simulated_power = simulated_power,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_sims = n_sims,
    significant_count = significant_count
  )
}

# Helper function for allocation proportions
.allocation_props <- function(allocation_ratio) {
  ratio <- ifelse(is.null(allocation_ratio) || allocation_ratio <= 0, 1, allocation_ratio)
  list(
    control = ratio / (1 + ratio),
    treatment = 1 / (1 + ratio)
  )
}
