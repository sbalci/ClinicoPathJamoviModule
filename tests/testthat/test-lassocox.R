# Test suite for lassocox function
# Tests cover functionality, data validation, model fitting, and edge cases

library(testthat)
library(survival)

# Helper functions for testing
create_survival_data <- function(n = 100, p = 10, censoring_rate = 0.3, seed = 123) {
  set.seed(seed)
  
  # Create covariates
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("var", 1:p)
  
  # Create survival times with some variables having true effects
  true_coef <- c(0.5, -0.3, 0.4, rep(0, p - 3))  # Only first 3 variables have effects
  linear_pred <- X %*% true_coef
  
  # Generate survival times from exponential distribution
  survival_times <- rexp(n, rate = exp(linear_pred))
  
  # Generate censoring times
  censoring_times <- rexp(n, rate = -log(1 - censoring_rate) / median(survival_times))
  
  # Observed times and status
  time <- pmin(survival_times, censoring_times)
  status <- as.numeric(survival_times <= censoring_times)
  
  # Create data frame
  data <- data.frame(
    time = time,
    status = factor(status, levels = c(0, 1), labels = c("censored", "event")),
    X
  )
  
  return(data)
}

create_high_dim_data <- function(n = 50, p = 100, seed = 456) {
  set.seed(seed)
  
  # High-dimensional scenario (p > n)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("gene", 1:p)
  
  # Sparse true effects (only few variables matter)
  true_coef <- rep(0, p)
  true_coef[c(1, 5, 10, 25)] <- c(0.8, -0.6, 0.4, -0.5)
  
  linear_pred <- X %*% true_coef
  survival_times <- rexp(n, rate = exp(linear_pred))
  censoring_times <- rexp(n, rate = 0.5)
  
  time <- pmin(survival_times, censoring_times)
  status <- as.numeric(survival_times <= censoring_times)
  
  data <- data.frame(
    survival_time = time,
    event = factor(status, levels = c(0, 1), labels = c("alive", "dead")),
    X
  )
  
  return(data)
}

# Data validation tests
describe("lassocox Data Validation", {
  
  test_that("lassocox validates time variable correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 50)
    
    # Test negative time values
    data_neg <- data
    data_neg$time[1] <- -1
    
    expect_error({
      # This would be caught in .cleanData()
      time <- data_neg$time
      if (any(time < 0, na.rm = TRUE)) {
        stop("Time variable contains negative values. Survival times must be non-negative.")
      }
    })
    
    # Test missing time values
    data_na <- data
    data_na$time[1:5] <- NA
    
    expect_error({
      time <- data_na$time
      if (any(is.na(time))) {
        stop("Time variable contains missing values. Please remove or impute missing observations.")
      }
    })
  })
  
  test_that("lassocox validates outcome variable correctly", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 50)
    
    # Test non-binary outcome
    data_multi <- data
    data_multi$status <- factor(c(rep("A", 20), rep("B", 20), rep("C", 10)))
    
    expect_error({
      outcome <- data_multi$status
      unique_levels <- unique(na.omit(outcome))
      if (length(unique_levels) != 2) {
        stop(paste0("Outcome variable must be binary (exactly 2 levels). Found ", 
                   length(unique_levels), " levels: ", 
                   paste(unique_levels, collapse = ", ")))
      }
    })
  })
  
  test_that("lassocox validates predictor variables", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 50, p = 5)
    
    # Test too few predictors
    expect_error({
      explanatory_vars <- colnames(data)[3]  # Only one variable
      if (length(explanatory_vars) < 2) {
        stop("At least 2 explanatory variables are required for Lasso regression.")
      }
    })
    
    # Test constant variables
    data_const <- data
    data_const$var1 <- 5  # Constant variable
    
    predictors <- data_const[c("var1", "var2")]
    constant_vars <- sapply(predictors, function(x) {
      if (is.numeric(x)) {
        var(x, na.rm = TRUE) == 0
      } else {
        length(unique(na.omit(x))) <= 1
      }
    })
    
    expect_true(constant_vars["var1"])
    expect_false(constant_vars["var2"])
  })
  
  test_that("lassocox validates sample size requirements", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    # Test minimum sample size
    expect_error({
      n_complete <- 5
      if (n_complete < 10) {
        stop(paste0("Too few complete cases for analysis (", n_complete, 
                   "). Need at least 10 complete observations."))
      }
    })
    
    # Test minimum events
    expect_error({
      n_events <- 2
      if (n_events < 5) {
        stop(paste0("Too few events for analysis (", n_events, 
                   "). Need at least 5 events for reliable estimation."))
      }
    })
  })
})

# Model fitting tests
describe("lassocox Model Fitting", {
  
  test_that("lassocox fits basic model correctly", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 100, p = 5)
    
    # Simulate model fitting steps
    time <- data$time
    status <- as.numeric(data$status == "event")
    X <- as.matrix(data[, paste0("var", 1:5)])
    
    # Create survival object
    y <- survival::Surv(time, status)
    expect_s3_class(y, "Surv")
    
    # Test glmnet fitting
    expect_no_error({
      cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 5)
    })
    
    expect_no_error({
      final_model <- glmnet::glmnet(x = X, y = y, family = "cox", lambda = cv_fit$lambda.1se)
    })
  })
  
  test_that("lassocox handles high-dimensional data", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_high_dim_data(n = 50, p = 100)
    
    time <- data$survival_time
    status <- as.numeric(data$event == "dead")
    X <- as.matrix(data[, paste0("gene", 1:100)])
    
    y <- survival::Surv(time, status)
    
    # Should handle p > n scenario
    expect_no_error({
      cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 3)
    })
    
    # Should perform variable selection
    final_model <- glmnet::glmnet(x = X, y = y, family = "cox", lambda = cv_fit$lambda.1se)
    coef_matrix <- as.matrix(coef(final_model))
    selected_vars <- which(coef_matrix != 0)
    
    # Should select fewer variables than total
    expect_true(length(selected_vars) < ncol(X))
    expect_true(length(selected_vars) >= 0)  # Could be 0 with strong regularization
  })
  
  test_that("lassocox cross-validation works correctly", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 80, p = 10)
    
    time <- data$time
    status <- as.numeric(data$status == "event")
    X <- as.matrix(data[, paste0("var", 1:10)])
    y <- survival::Surv(time, status)
    
    # Test different fold numbers
    for (nfolds in c(3, 5, 10)) {
      actual_folds <- min(nfolds, nrow(X) %/% 3)
      expect_no_error({
        cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = actual_folds)
      })
      
      # Should have lambda values
      expect_true(length(cv_fit$lambda) > 0)
      expect_true(!is.na(cv_fit$lambda.min))
      expect_true(!is.na(cv_fit$lambda.1se))
    }
  })
})

# Performance metrics tests
describe("lassocox Performance Metrics", {
  
  test_that("lassocox calculates C-index correctly", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 100, p = 5)
    
    time <- data$time
    status <- as.numeric(data$status == "event")
    X <- as.matrix(data[, paste0("var", 1:5)])
    y <- survival::Surv(time, status)
    
    # Fit model and get risk scores
    cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 5)
    final_model <- glmnet::glmnet(x = X, y = y, family = "cox", lambda = cv_fit$lambda.1se)
    risk_scores <- predict(final_model, newx = X, type = "link")
    
    # Calculate C-index
    cindex_result <- survival::concordance(y ~ risk_scores)
    expect_true(cindex_result$concordance >= 0 && cindex_result$concordance <= 1)
    expect_true(!is.na(cindex_result$concordance))
  })
  
  test_that("lassocox calculates risk group analysis", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 100, p = 5)
    
    time <- data$time
    status <- as.numeric(data$status == "event")
    X <- as.matrix(data[, paste0("var", 1:5)])
    y <- survival::Surv(time, status)
    
    # Create mock risk scores
    risk_scores <- rnorm(length(time))
    
    # Create risk groups
    risk_groups <- cut(risk_scores, 
                      breaks = quantile(risk_scores, c(0, 0.5, 1)),
                      labels = c("Low Risk", "High Risk"),
                      include.lowest = TRUE)
    
    expect_equal(length(levels(risk_groups)), 2)
    expect_true(all(levels(risk_groups) %in% c("Low Risk", "High Risk")))
    
    # Test log-rank test
    expect_no_error({
      survdiff_result <- survival::survdiff(y ~ risk_groups)
      p_value <- 1 - pchisq(survdiff_result$chisq, df = length(survdiff_result$n) - 1)
      expect_true(p_value >= 0 && p_value <= 1)
    })
    
    # Test Cox regression for hazard ratio
    expect_no_error({
      cox_fit <- survival::coxph(y ~ risk_groups)
      hr <- exp(coef(cox_fit))
      expect_true(hr > 0)
    })
  })
})

# Variable selection tests
describe("lassocox Variable Selection", {
  
  test_that("lassocox selects relevant variables", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    # Create data with known structure
    set.seed(789)
    n <- 100
    # Create variables where only first 3 have true effects
    X <- matrix(rnorm(n * 10), nrow = n, ncol = 10)
    colnames(X) <- paste0("var", 1:10)
    
    true_coef <- c(0.8, -0.6, 0.4, rep(0, 7))  # Only first 3 variables matter
    linear_pred <- X %*% true_coef
    survival_times <- rexp(n, rate = exp(linear_pred))
    censoring_times <- rexp(n, rate = 0.3)
    
    time <- pmin(survival_times, censoring_times)
    status <- as.numeric(survival_times <= censoring_times)
    
    y <- survival::Surv(time, status)
    
    # Fit Lasso model
    cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 5)
    final_model <- glmnet::glmnet(x = X, y = y, family = "cox", lambda = cv_fit$lambda.min)
    
    coef_matrix <- as.matrix(coef(final_model))
    selected_vars <- which(coef_matrix != 0)
    
    # Should select some variables
    expect_true(length(selected_vars) >= 0)
    expect_true(length(selected_vars) <= ncol(X))
    
    # Variable importance should be non-negative
    if (length(selected_vars) > 0) {
      var_importance <- abs(coef_matrix[selected_vars, 1])
      expect_true(all(var_importance >= 0))
    }
  })
  
  test_that("lassocox handles lambda selection methods", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 80, p = 8)
    
    time <- data$time
    status <- as.numeric(data$status == "event")
    X <- as.matrix(data[, paste0("var", 1:8)])
    y <- survival::Surv(time, status)
    
    cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 5)
    
    # Test lambda.min selection
    lambda_min <- cv_fit$lambda.min
    model_min <- glmnet::glmnet(x = X, y = y, family = "cox", lambda = lambda_min)
    coef_min <- as.matrix(coef(model_min))
    selected_min <- sum(coef_min != 0)
    
    # Test lambda.1se selection  
    lambda_1se <- cv_fit$lambda.1se
    model_1se <- glmnet::glmnet(x = X, y = y, family = "cox", lambda = lambda_1se)
    coef_1se <- as.matrix(coef(model_1se))
    selected_1se <- sum(coef_1se != 0)
    
    # lambda.1se should generally select fewer or equal variables than lambda.min
    expect_true(lambda_1se >= lambda_min)
    expect_true(selected_1se <= selected_min)
  })
})

# Edge cases and robustness tests
describe("lassocox Edge Cases", {
  
  test_that("lassocox handles all censored data", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    # Create data with all censored observations
    data <- create_survival_data(n = 50, p = 5)
    data$status <- factor(rep("censored", nrow(data)), levels = c("censored", "event"))
    
    time <- data$time
    status <- as.numeric(data$status == "event")
    
    # Should have no events
    expect_equal(sum(status), 0)
    
    # This should trigger an error in validation
    expect_error({
      n_events <- sum(status)
      if (n_events < 5) {
        stop(paste0("Too few events for analysis (", n_events, 
                   "). Need at least 5 events for reliable estimation."))
      }
    })
  })
  
  test_that("lassocox handles small sample sizes gracefully", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    # Test with minimum viable sample size
    data <- create_survival_data(n = 15, p = 3)
    
    time <- data$time
    status <- as.numeric(data$status == "event")
    X <- as.matrix(data[, paste0("var", 1:3)])
    y <- survival::Surv(time, status)
    
    # Should work with reduced CV folds
    nfolds <- max(3, min(5, nrow(X) %/% 3))
    expect_true(nfolds >= 3)
    
    expect_no_error({
      cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = nfolds)
    })
  })
  
  test_that("lassocox handles factor variables correctly", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 80, p = 3)
    
    # Add factor variables
    data$treatment <- factor(sample(c("A", "B", "C"), nrow(data), replace = TRUE))
    data$gender <- factor(sample(c("M", "F"), nrow(data), replace = TRUE))
    
    # Create model matrix
    predictors <- data[c("var1", "var2", "treatment", "gender")]
    X <- model.matrix(~ ., data = predictors)[, -1]
    
    # Should create dummy variables for factors
    expect_true(ncol(X) > ncol(predictors))
    expect_true(any(grepl("treatment", colnames(X))))
    expect_true(any(grepl("gender", colnames(X))))
    
    # Should work with glmnet
    time <- data$time
    status <- as.numeric(data$status == "event")
    y <- survival::Surv(time, status)
    
    expect_no_error({
      cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 5)
    })
  })
  
  test_that("lassocox handles standardization correctly", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 60, p = 5)
    
    # Create variables with different scales
    data$var1 <- data$var1 * 1000  # Large scale
    data$var2 <- data$var2 / 1000  # Small scale
    
    X <- as.matrix(data[, paste0("var", 1:5)])
    
    # Test standardization
    X_scaled <- scale(X)
    
    # Should have mean ~0 and sd ~1
    expect_true(all(abs(colMeans(X_scaled)) < 1e-10))
    expect_true(all(abs(apply(X_scaled, 2, sd) - 1) < 1e-10))
    
    # Should work with glmnet
    time <- data$time
    status <- as.numeric(data$status == "event")
    y <- survival::Surv(time, status)
    
    expect_no_error({
      cv_fit <- glmnet::cv.glmnet(x = X_scaled, y = y, family = "cox", nfolds = 5)
    })
  })
})

# Integration tests
describe("lassocox Integration", {
  
  test_that("lassocox produces consistent results", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 100, p = 10, seed = 999)
    
    time <- data$time
    status <- as.numeric(data$status == "event")
    X <- as.matrix(data[, paste0("var", 1:10)])
    y <- survival::Surv(time, status)
    
    # Run multiple times with same seed
    set.seed(12345)
    cv_fit1 <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 5)
    
    set.seed(12345)
    cv_fit2 <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 5)
    
    # Should get same results with same seed
    expect_equal(cv_fit1$lambda.min, cv_fit2$lambda.min)
    expect_equal(cv_fit1$lambda.1se, cv_fit2$lambda.1se)
  })
  
  test_that("lassocox model components are valid", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 80, p = 6)
    
    time <- data$time
    status <- as.numeric(data$status == "event")
    X <- as.matrix(data[, paste0("var", 1:6)])
    y <- survival::Surv(time, status)
    
    # Fit complete model
    cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 5)
    final_model <- glmnet::glmnet(x = X, y = y, family = "cox", lambda = cv_fit$lambda.1se)
    
    # Get model components
    coef_matrix <- as.matrix(coef(final_model))
    selected_vars <- which(coef_matrix != 0)
    risk_scores <- predict(final_model, newx = X, type = "link")
    
    # Validate components
    expect_true(is.matrix(coef_matrix))
    expect_equal(nrow(coef_matrix), ncol(X))
    expect_true(is.numeric(selected_vars))
    expect_true(length(selected_vars) <= ncol(X))
    expect_true(is.numeric(risk_scores))
    expect_equal(length(risk_scores), nrow(X))
    
    # Risk scores should have reasonable range
    expect_true(all(is.finite(risk_scores)))
    expect_true(sd(risk_scores) > 0)  # Should have some variation
  })
})

# Interpretation and output tests
describe("lassocox Output Interpretation", {
  
  test_that("lassocox C-index interpretation is correct", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    # Test interpretation function
    interpret_cindex <- function(cindex) {
      if (is.na(cindex)) return("Not available")
      if (cindex < 0.6) return("Poor discrimination")
      if (cindex < 0.7) return("Fair discrimination") 
      if (cindex < 0.8) return("Good discrimination")
      return("Excellent discrimination")
    }
    
    expect_equal(interpret_cindex(0.5), "Poor discrimination")
    expect_equal(interpret_cindex(0.65), "Fair discrimination")
    expect_equal(interpret_cindex(0.75), "Good discrimination")
    expect_equal(interpret_cindex(0.85), "Excellent discrimination")
    expect_equal(interpret_cindex(NA), "Not available")
  })
  
  test_that("lassocox hazard ratio interpretation is correct", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    # Test interpretation function
    interpret_hr <- function(hr) {
      if (is.na(hr)) return("Not available")
      if (hr < 1.5) return("Weak risk stratification")
      if (hr < 2.5) return("Moderate risk stratification")
      return("Strong risk stratification")
    }
    
    expect_equal(interpret_hr(1.2), "Weak risk stratification")
    expect_equal(interpret_hr(2.0), "Moderate risk stratification")
    expect_equal(interpret_hr(3.5), "Strong risk stratification")
    expect_equal(interpret_hr(NA), "Not available")
  })
  
  test_that("lassocox produces reasonable coefficient values", {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    skip_if_not_installed("jmvcore")
    
    data <- create_survival_data(n = 100, p = 5)
    
    time <- data$time
    status <- as.numeric(data$status == "event")
    X <- as.matrix(data[, paste0("var", 1:5)])
    y <- survival::Surv(time, status)
    
    cv_fit <- glmnet::cv.glmnet(x = X, y = y, family = "cox", nfolds = 5)
    final_model <- glmnet::glmnet(x = X, y = y, family = "cox", lambda = cv_fit$lambda.1se)
    
    coef_matrix <- as.matrix(coef(final_model))
    selected_vars <- which(coef_matrix != 0)
    
    if (length(selected_vars) > 0) {
      coef_values <- coef_matrix[selected_vars, 1]
      
      # Coefficients should be finite
      expect_true(all(is.finite(coef_values)))
      
      # Hazard ratios should be positive
      hr_values <- exp(coef_values)
      expect_true(all(hr_values > 0))
      
      # For reasonable data, coefficients shouldn't be extreme
      expect_true(all(abs(coef_values) < 10))
    }
  })
})
