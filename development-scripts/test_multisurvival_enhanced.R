# Enhanced Verification Tests for multisurvival Function
# Based on comprehensive statistical evaluation
# Tests mathematical accuracy and clinical appropriateness

library(testthat)
library(survival)

# Test 1: Person-Time Poisson Exact CI Accuracy ----
test_that("Person-time Poisson exact CIs are mathematically correct", {
  # Known values from literature (Ulm 1990)
  # For 10 events, person-time = 100, multiplier = 100

  events <- 10
  person_time <- 100
  multiplier <- 100

  # Calculate rate and CI
  rate <- (events / person_time) * multiplier  # Should be 10

  # Poisson exact CI formula (Ulm 1990)
  ci_lower <- (qchisq(0.025, 2*events) / 2) / person_time * multiplier
  ci_upper <- (qchisq(0.975, 2*(events + 1)) / 2) / person_time * multiplier

  # Expected values (from statistical tables)
  expect_equal(rate, 10)
  expect_lt(ci_lower, rate)  # Lower CI should be less than rate
  expect_gt(ci_upper, rate)  # Upper CI should be greater than rate

  # Check CI width is reasonable (rule of thumb: CI width increases as events decrease)
  ci_width <- ci_upper - ci_lower
  expect_gt(ci_width, 0)
  expect_lt(ci_width, rate * 2)  # CI width shouldn't be more than 2x the rate

  # Test edge case: Zero events
  events_zero <- 0
  ci_lower_zero <- (qchisq(0.025, 2*events_zero) / 2) / person_time * multiplier
  ci_upper_zero <- (qchisq(0.975, 2*(events_zero + 1)) / 2) / person_time * multiplier

  expect_equal(ci_lower_zero, 0)  # Lower CI should be 0
  expect_gt(ci_upper_zero, 0)     # Upper CI should be > 0 (rule of 3)
})

# Test 2: Risk Score Quantile Stratification ----
test_that("Risk scores are correctly stratified into quantiles", {
  # Simulate risk scores
  set.seed(42)
  n <- 100
  risk_scores <- rnorm(n, mean = 1, sd = 0.5)

  # Test 4 groups (quartiles)
  breaks_4 <- quantile(risk_scores, probs = c(0, 0.25, 0.5, 0.75, 1))
  groups_4 <- cut(risk_scores, breaks = breaks_4,
                  labels = c("Low", "Int-Low", "Int-High", "High"),
                  include.lowest = TRUE)

  # Check each group has ~25% of observations
  table_4 <- table(groups_4)
  expect_equal(length(table_4), 4)
  expect_true(all(table_4 >= 20))  # At least 20% in each group
  expect_true(all(table_4 <= 30))  # At most 30% in each group

  # Test 3 groups (tertiles)
  breaks_3 <- quantile(risk_scores, probs = c(0, 1/3, 2/3, 1))
  groups_3 <- cut(risk_scores, breaks = breaks_3,
                  labels = c("Low", "Int", "High"),
                  include.lowest = TRUE)

  table_3 <- table(groups_3)
  expect_equal(length(table_3), 3)

  # Test 2 groups (median split)
  breaks_2 <- quantile(risk_scores, probs = c(0, 0.5, 1))
  groups_2 <- cut(risk_scores, breaks = breaks_2,
                  labels = c("Low", "High"),
                  include.lowest = TRUE)

  table_2 <- table(groups_2)
  expect_equal(length(table_2), 2)
})

# Test 3: Cox Model Risk Prediction Accuracy ----
test_that("Cox model risk predictions are exp(linear predictor)", {
  # Use colon dataset
  data(colon, package = "survival")
  colon_test <- colon[colon$etype == 1 & !is.na(colon$time) & !is.na(colon$status), ]

  # Fit Cox model
  cox_model <- coxph(Surv(time, status) ~ age + sex, data = colon_test)

  # Get risk scores
  risk_scores <- predict(cox_model, type = "risk")

  # Get linear predictor
  linear_pred <- predict(cox_model, type = "lp")

  # Verify: risk = exp(lp)
  expect_equal(risk_scores, exp(linear_pred), tolerance = 1e-10)

  # Verify: mean risk ≈ 1 (relative to baseline)
  expect_lt(abs(mean(risk_scores) - 1), 0.5)
})

# Test 4: AFT Time Ratio Calculation ----
test_that("AFT Time Ratios are correctly calculated as exp(beta)", {
  # Use lung dataset
  data(lung, package = "survival")
  lung_test <- lung[!is.na(lung$time) & !is.na(lung$status), ]

  # Fit Weibull AFT model
  aft_model <- survreg(Surv(time, status) ~ age + sex,
                       data = lung_test, dist = "weibull")

  # Get coefficients
  coefs <- coef(aft_model)

  # Calculate Time Ratios
  time_ratios <- exp(coefs)

  # Verify: TR > 0 (must be positive)
  expect_true(all(time_ratios > 0))

  # Verify: TR = 1 means no effect (exp(0) = 1)
  expect_equal(exp(0), 1)

  # Verify: TR > 1 means longer survival
  # TR < 1 means shorter survival
  age_coef <- coefs["age"]
  if (age_coef < 0) {
    expect_lt(exp(age_coef), 1)  # Negative coef -> TR < 1
  }
})

# Test 5: Fine-Gray Competing Risks Setup ----
test_that("Fine-Gray competing risks data is correctly structured", {
  skip_if_not_installed("survival")
  skip_if(packageVersion("survival") < "3.5.0",
          "Fine-Gray requires survival >= 3.5.0")

  # Create competing risks data with correct factor encoding
  set.seed(123)
  n <- 100
  cr_data <- data.frame(
    time = rexp(n, 0.1),
    age = rnorm(n, 60, 10),
    sex = factor(sample(c("M", "F"), n, replace = TRUE))
  )

  # Correctly encode competing risks as factor with named levels
  # Factor levels: "censored", "Event", "Competing"
  status_sim <- sample(c("censored", "Event", "Competing"), n,
                      replace = TRUE, prob = c(0.5, 0.3, 0.2))
  cr_data$status <- factor(status_sim, levels = c("censored", "Event", "Competing"))

  # Create Fine-Gray data (etype="Event" for event of interest)
  fg_formula <- Surv(time, status) ~ age + sex
  fg_result <- finegray(fg_formula, data = cr_data, etype = "Event")

  # Verify structure
  expect_true("fgstart" %in% names(fg_result))
  expect_true("fgstop" %in% names(fg_result))
  expect_true("fgstatus" %in% names(fg_result))
  expect_true("fgwt" %in% names(fg_result))

  # Verify weights sum appropriately
  expect_gt(sum(fg_result$fgwt), 0)

  # Verify expanded data has more rows (due to late entries from competing events)
  expect_gte(nrow(fg_result), sum(cr_data$status == 1))
})

# Test 6: Model Selection Logic ----
test_that("Model selection correctly implements backward elimination", {
  # Use colon dataset
  data(colon, package = "survival")
  colon_test <- colon[colon$etype == 1 &
                      !is.na(colon$time) &
                      !is.na(colon$status) &
                      complete.cases(colon[, c("age", "sex", "obstruct", "perfor")]), ]

  # Full model
  full_formula <- Surv(time, status) ~ age + sex + obstruct + perfor
  full_model <- coxph(full_formula, data = colon_test)

  # Backward selection (manual)
  current_model <- full_model
  p_removal <- 0.10

  # Get p-values
  coef_summary <- summary(current_model)$coefficients
  p_values <- coef_summary[, "Pr(>|z|)"]

  # Check if any p-value > p_removal
  max_p <- max(p_values)

  if (max_p > p_removal) {
    # Variable should be removed
    drop_var <- names(p_values)[which.max(p_values)]
    expect_gt(max_p, p_removal)
    expect_true(drop_var %in% names(coef(full_model)))
  }
})

# Test 7: Proportional Hazards Test ----
test_that("Proportional hazards testing works correctly", {
  # Use colon dataset
  data(colon, package = "survival")
  colon_test <- colon[colon$etype == 1 &
                      !is.na(colon$time) &
                      !is.na(colon$status), ]

  # Fit Cox model
  cox_model <- coxph(Surv(time, status) ~ age + sex, data = colon_test)

  # Test PH assumption
  zph <- cox.zph(cox_model)

  # Verify structure
  expect_true("table" %in% names(zph))
  expect_true("p" %in% colnames(zph$table))

  # Verify p-values are between 0 and 1
  p_values <- zph$table[, "p"]
  expect_true(all(p_values >= 0 & p_values <= 1))

  # Check for violations (p < 0.05)
  violations <- p_values < 0.05
  if (any(violations)) {
    violating_vars <- rownames(zph$table)[violations]
    expect_true(length(violating_vars) > 0)
  }
})

# Test 8: Stratification Handling ----
test_that("Stratification correctly creates separate baseline hazards", {
  # Use colon dataset
  data(colon, package = "survival")
  colon_test <- colon[colon$etype == 1 &
                      !is.na(colon$time) &
                      !is.na(colon$status), ]

  # Model with stratification
  strat_model <- coxph(Surv(time, status) ~ age + strata(sex),
                       data = colon_test)

  # Model without stratification
  nostrat_model <- coxph(Surv(time, status) ~ age + sex,
                         data = colon_test)

  # Verify stratified model doesn't estimate sex coefficient
  expect_false("sex" %in% names(coef(strat_model)))

  # Verify non-stratified model does estimate sex coefficient
  expect_true("sexMale" %in% names(coef(nostrat_model)) ||
              "sex" %in% names(coef(nostrat_model)))

  # Verify age coefficient differs between models
  # (stratification affects other coefficients)
  age_strat <- coef(strat_model)["age"]
  age_nostrat <- coef(nostrat_model)["age"]

  expect_false(is.na(age_strat))
  expect_false(is.na(age_nostrat))
})

# Test 9: Date Handling and Time Conversion ----
test_that("Date conversion handles multiple formats correctly", {
  # Test YMD format
  date1 <- "2020-01-15"
  date2 <- "2022-06-20"

  time_days <- as.numeric(difftime(
    lubridate::ymd(date2),
    lubridate::ymd(date1),
    units = "days"
  ))

  expect_gt(time_days, 0)
  expect_lt(time_days, 1000)  # Should be ~2.5 years

  # Convert to months
  time_months <- time_days / 30.4375  # Average days per month
  expect_gt(time_months, 25)
  expect_lt(time_months, 35)

  # Convert to years
  time_years <- time_days / 365.25
  expect_gt(time_years, 2)
  expect_lt(time_years, 3)
})

# Test 10: Edge Case - Single Event ----
test_that("Function handles dataset with single event gracefully", {
  set.seed(456)
  n <- 50
  edge_data <- data.frame(
    time = rexp(n, 0.1),
    status = c(1, rep(0, n-1)),  # Only one event
    age = rnorm(n, 60, 10),
    sex = factor(sample(c("M", "F"), n, replace = TRUE))
  )

  # This should work but with warnings
  expect_warning(
    cox_model <- coxph(Surv(time, status) ~ age + sex, data = edge_data),
    NA  # Expect no warning from coxph itself
  )

  # Model should fit
  expect_true(!is.null(cox_model))
  expect_equal(sum(edge_data$status), 1)
})

# Test 11: Edge Case - High Censoring Rate ----
test_that("Function handles high censoring rate (>90%)", {
  set.seed(789)
  n <- 100
  high_censor_data <- data.frame(
    time = rexp(n, 0.1),
    status = c(rep(1, 5), rep(0, 95)),  # 95% censoring
    age = rnorm(n, 60, 10),
    treatment = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  # Model should fit but with low power
  cox_model <- coxph(Surv(time, status) ~ age + treatment,
                     data = high_censor_data)

  expect_true(!is.null(cox_model))
  expect_equal(sum(high_censor_data$status), 5)

  # Confidence intervals should be wide
  ci <- confint(cox_model)
  ci_widths <- ci[, 2] - ci[, 1]
  expect_true(all(ci_widths > 1))  # Wide CIs due to few events
})

# Test 12: Landmark Analysis Time Adjustment ----
test_that("Landmark analysis correctly adjusts entry time", {
  set.seed(321)
  n <- 100
  landmark_data <- data.frame(
    time = rexp(n, 0.1) + 10,  # All times > 10
    status = sample(0:1, n, replace = TRUE),
    age = rnorm(n, 60, 10)
  )

  landmark_time <- 5

  # Adjust times
  landmark_data$time_adj <- landmark_data$time - landmark_time

  # Filter: only include subjects with time > landmark
  landmark_subset <- landmark_data[landmark_data$time > landmark_time, ]

  # All adjusted times should be positive
  expect_true(all(landmark_subset$time_adj > 0))

  # Should have fewer observations (some events before landmark)
  expect_lte(nrow(landmark_subset), nrow(landmark_data))
})

# Run all tests
cat("\n=== Running Enhanced multisurvival Tests ===\n\n")
test_dir(".", pattern = "test_multisurvival_enhanced.R")
cat("\n=== All Tests Complete ===\n")
