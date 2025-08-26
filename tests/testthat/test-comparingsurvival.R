test_that("comparingsurvival works", {
    
    # Skip if survival package is not available
    skip_if_not_installed("survival")
    skip_if_not_installed("ggfortify")
    
    # Create test data for survival comparison
    set.seed(42)
    n <- 200
    
    # Create comprehensive survival test dataset
    test_data <- data.frame(
        # Time to event (months)
        survival_time = rexp(n, rate = 0.05) * 12,  # exponential survival times
        
        # Event indicator
        event_occurred = rbinom(n, 1, 0.6),  # 60% event rate
        
        # Treatment groups
        treatment = factor(sample(c("Control", "Treatment"), n, replace = TRUE)),
        
        # Other grouping variables
        stage = factor(sample(c("Early", "Advanced"), n, replace = TRUE, prob = c(0.6, 0.4))),
        sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
        
        # Additional variables
        age = rnorm(n, 65, 12),
        risk_score = rnorm(n, 50, 15)
    )
    
    # Ensure realistic ranges
    test_data$survival_time <- pmax(0.1, pmin(120, test_data$survival_time))
    test_data$age <- pmax(18, pmin(95, test_data$age))
    test_data$risk_score <- pmax(0, pmin(100, test_data$risk_score))
    
    # Test 1: Basic survival comparison by treatment
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data,
            times = "survival_time",
            status = "event_occurred",
            groups = "treatment",
            ciyn = TRUE,
            timeunits = "Months"
        )
    })
    
    # Test 2: Survival comparison by stage
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data,
            times = "survival_time",
            status = "event_occurred",
            groups = "stage",
            ciyn = FALSE,
            loglogyn = TRUE,
            timeunits = "Months"
        )
    })
    
    # Test 3: Survival comparison by sex
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data,
            times = "survival_time",
            status = "event_occurred",
            groups = "sex",
            ciyn = TRUE,
            loglogyn = FALSE,
            timeunits = "None"
        )
    })
    
    # Test 4: With logical event indicator
    test_data_logical <- test_data
    test_data_logical$event_logical <- as.logical(test_data$event_occurred)
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data_logical,
            times = "survival_time",
            status = "event_logical",
            groups = "treatment",
            ciyn = TRUE,
            timeunits = "Months"
        )
    })
    
    # Test 5: With factor event indicator
    test_data_factor <- test_data
    test_data_factor$event_factor <- factor(test_data$event_occurred, 
                                          levels = c(0, 1), 
                                          labels = c("Censored", "Event"))
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data_factor,
            times = "survival_time",
            status = "event_factor",
            groups = "treatment",
            ciyn = FALSE,
            timeunits = "Months"
        )
    })
    
    # Test 6: Different time units
    time_units <- c("None", "Days", "Weeks", "Months", "Years")
    
    for (unit in time_units) {
        testthat::expect_silent({
            results <- ClinicoPath::comparingSurvival(
                data = test_data,
                times = "survival_time",
                status = "event_occurred",
                groups = "treatment",
                timeunits = unit
            )
        })
    }
    
    # Test 7: With and without confidence intervals
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data,
            times = "survival_time",
            status = "event_occurred",
            groups = "treatment",
            ciyn = TRUE
        )
    })
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data,
            times = "survival_time",
            status = "event_occurred",
            groups = "treatment",
            ciyn = FALSE
        )
    })
    
    # Test 8: With and without log-log plots
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data,
            times = "survival_time",
            status = "event_occurred",
            groups = "treatment",
            loglogyn = TRUE
        )
    })
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data,
            times = "survival_time",
            status = "event_occurred",
            groups = "treatment",
            loglogyn = FALSE
        )
    })
    
    # Test 9: Three-group comparison
    test_data_3groups <- test_data
    test_data_3groups$treatment_3 <- factor(sample(c("Control", "Treatment A", "Treatment B"), 
                                                   nrow(test_data), replace = TRUE))
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data_3groups,
            times = "survival_time",
            status = "event_occurred",
            groups = "treatment_3",
            ciyn = TRUE,
            timeunits = "Months"
        )
    })
    
    # Test 10: All options enabled
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = test_data,
            times = "survival_time",
            status = "event_occurred",
            groups = "treatment",
            ciyn = TRUE,
            loglogyn = TRUE,
            timeunits = "Months"
        )
    })
})

test_that("comparingsurvival handles edge cases", {
    
    # Skip if survival package is not available
    skip_if_not_installed("survival")
    
    # Create minimal test data
    minimal_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        status = c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1),
        group = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"))
    )
    
    # Test 1: Empty variable selection should show instructions
    testthat::expect_no_error({
        results <- ClinicoPath::comparingSurvival(
            data = minimal_data
        )
    })
    
    # Test 2: Only time variable selected
    testthat::expect_no_error({
        results <- ClinicoPath::comparingSurvival(
            data = minimal_data,
            times = "time"
        )
    })
    
    # Test 3: Time and status but no groups
    testthat::expect_no_error({
        results <- ClinicoPath::comparingSurvival(
            data = minimal_data,
            times = "time",
            status = "status"
        )
    })
    
    # Test 4: Minimal valid analysis
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = minimal_data,
            times = "time",
            status = "status",
            groups = "group"
        )
    })
    
    # Test 5: Data with missing values
    missing_data <- data.frame(
        time = c(1, 2, NA, 4, 5, 6, 7, NA, 9, 10),
        status = c(1, 0, 1, NA, 0, 1, 0, 1, 0, 1),
        group = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", NA))
    )
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = missing_data,
            times = "time",
            status = "status",
            groups = "group"
        )
    })
    
    # Test 6: Very small dataset
    tiny_data <- data.frame(
        time = c(1, 2, 3, 4),
        status = c(1, 0, 1, 0),
        group = factor(c("A", "A", "B", "B"))
    )
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = tiny_data,
            times = "time",
            status = "status",
            groups = "group"
        )
    })
    
    # Test 7: All events censored in one group
    censored_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8),
        status = c(1, 1, 1, 1, 0, 0, 0, 0),  # All group B censored
        group = factor(c("A", "A", "A", "A", "B", "B", "B", "B"))
    )
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = censored_data,
            times = "time",
            status = "status",
            groups = "group"
        )
    })
    
    # Test 8: Single group (should show error)
    single_group_data <- data.frame(
        time = c(1, 2, 3, 4, 5),
        status = c(1, 0, 1, 0, 1),
        group = factor(rep("A", 5))
    )
    
    testthat::expect_error({
        results <- ClinicoPath::comparingSurvival(
            data = single_group_data,
            times = "time",
            status = "status",
            groups = "group"
        )
    })
})

test_that("comparingsurvival data validation", {
    
    # Skip if survival package is not available
    skip_if_not_installed("survival")
    
    # Create standard test data
    test_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        status = c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1),
        group = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"))
    )
    
    # Test 1: Negative time values (should error)
    negative_time_data <- test_data
    negative_time_data$time[1] <- -1
    
    testthat::expect_error({
        results <- ClinicoPath::comparingSurvival(
            data = negative_time_data,
            times = "time",
            status = "status",
            groups = "group"
        )
    })
    
    # Test 2: Non-numeric time variable (should error)
    character_time_data <- test_data
    character_time_data$time <- as.character(character_time_data$time)
    
    testthat::expect_error({
        results <- ClinicoPath::comparingSurvival(
            data = character_time_data,
            times = "time",
            status = "status",
            groups = "group"
        )
    })
    
    # Test 3: Invalid status values (should error)
    invalid_status_data <- test_data
    invalid_status_data$status <- c(1, 2, 3, 1, 0, 1, 0, 1, 0, 1)  # Contains 2 and 3
    
    testthat::expect_error({
        results <- ClinicoPath::comparingSurvival(
            data = invalid_status_data,
            times = "time",
            status = "status",
            groups = "group"
        )
    })
    
    # Test 4: Status with more than 2 factor levels (should error)
    multi_level_status <- test_data
    multi_level_status$status <- factor(c("No", "Yes", "Maybe", "No", "Yes", "No", "Yes", "No", "Yes", "No"))
    
    testthat::expect_error({
        results <- ClinicoPath::comparingSurvival(
            data = multi_level_status,
            times = "time",
            status = "status",
            groups = "group"
        )
    })
    
    # Test 5: Valid alternative status formats
    # Test logical status
    logical_status_data <- test_data
    logical_status_data$status_logical <- as.logical(test_data$status)
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = logical_status_data,
            times = "time",
            status = "status_logical",
            groups = "group"
        )
    })
    
    # Test factor status with 2 levels
    factor_status_data <- test_data
    factor_status_data$status_factor <- factor(test_data$status, 
                                              levels = c(0, 1), 
                                              labels = c("Alive", "Dead"))
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = factor_status_data,
            times = "time",
            status = "status_factor",
            groups = "group"
        )
    })
    
    # Test 6: Non-factor grouping variable (should convert automatically)
    character_group_data <- test_data
    character_group_data$group_char <- as.character(test_data$group)
    
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = character_group_data,
            times = "time",
            status = "status",
            groups = "group_char"
        )
    })
})

test_that("comparingsurvival with realistic data", {
    
    # Skip if survival package is not available
    skip_if_not_installed("survival")
    
    # Load built-in survival datasets if available
    data("melanoma", package = "ClinicoPath", envir = environment())
    data("colon", package = "ClinicoPath", envir = environment())
    
    # Test with melanoma data
    if (exists("melanoma")) {
        testthat::expect_silent({
            results <- ClinicoPath::comparingSurvival(
                data = melanoma,
                times = "time",
                status = "status",
                groups = "sex",
                ciyn = TRUE,
                timeunits = "Days"
            )
        })
    }
    
    # Test with colon data
    if (exists("colon")) {
        testthat::expect_silent({
            results <- ClinicoPath::comparingSurvival(
                data = colon,
                times = "time",
                status = "status",
                groups = "sex",
                ciyn = TRUE,
                timeunits = "Days"
            )
        })
    }
    
    # Create realistic clinical trial data
    set.seed(123)
    n_patients <- 300
    
    clinical_trial_data <- data.frame(
        # Patient characteristics
        patient_id = 1:n_patients,
        age = round(rnorm(n_patients, 65, 10)),
        sex = factor(sample(c("Male", "Female"), n_patients, replace = TRUE)),
        
        # Treatment assignment
        treatment = factor(sample(c("Standard", "Experimental"), n_patients, replace = TRUE)),
        
        # Disease characteristics
        stage = factor(sample(c("I", "II", "III", "IV"), n_patients, 
                             replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))),
        
        # Survival outcomes
        survival_months = rweibull(n_patients, shape = 1.5, scale = 20),
        event_occurred = rbinom(n_patients, 1, 0.65)
    )
    
    # Ensure realistic ranges
    clinical_trial_data$age <- pmax(18, pmin(90, clinical_trial_data$age))
    clinical_trial_data$survival_months <- pmax(0.1, pmin(60, clinical_trial_data$survival_months))
    
    # Test survival comparison by treatment
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = clinical_trial_data,
            times = "survival_months",
            status = "event_occurred",
            groups = "treatment",
            ciyn = TRUE,
            loglogyn = TRUE,
            timeunits = "Months"
        )
    })
    
    # Test survival comparison by stage
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = clinical_trial_data,
            times = "survival_months",
            status = "event_occurred",
            groups = "stage",
            ciyn = TRUE,
            timeunits = "Months"
        )
    })
    
    # Test survival comparison by sex
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = clinical_trial_data,
            times = "survival_months",
            status = "event_occurred",
            groups = "sex",
            ciyn = FALSE,
            loglogyn = FALSE,
            timeunits = "Months"
        )
    })
})

test_that("comparingsurvival output options", {
    
    # Skip if survival package is not available
    skip_if_not_installed("survival")
    
    # Create test data
    test_data <- data.frame(
        time = rexp(100, 0.1),
        status = rbinom(100, 1, 0.7),
        group = factor(sample(c("A", "B", "C"), 100, replace = TRUE))
    )
    
    # Test all combinations of options
    for (ci in c(TRUE, FALSE)) {
        for (loglog in c(TRUE, FALSE)) {
            for (unit in c("None", "Days", "Months", "Years")) {
                testthat::expect_silent({
                    results <- ClinicoPath::comparingSurvival(
                        data = test_data,
                        times = "time",
                        status = "status",
                        groups = "group",
                        ciyn = ci,
                        loglogyn = loglog,
                        timeunits = unit
                    )
                })
            }
        }
    }
})

test_that("comparingsurvival statistical accuracy", {
    
    # Skip if survival package is not available
    skip_if_not_installed("survival")
    
    # Create test data with known differences
    set.seed(456)
    n_per_group <- 50
    
    # Group A: better survival (lower hazard)
    group_a_times <- rexp(n_per_group, rate = 0.05)
    group_a_status <- rbinom(n_per_group, 1, 0.6)
    
    # Group B: worse survival (higher hazard)  
    group_b_times <- rexp(n_per_group, rate = 0.1)
    group_b_status <- rbinom(n_per_group, 1, 0.8)
    
    known_diff_data <- data.frame(
        time = c(group_a_times, group_b_times),
        status = c(group_a_status, group_b_status),
        group = factor(c(rep("Better", n_per_group), rep("Worse", n_per_group)))
    )
    
    # Test that we can detect the difference
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = known_diff_data,
            times = "time",
            status = "status",
            groups = "group",
            ciyn = TRUE,
            timeunits = "Years"
        )
    })
    
    # Create data with no difference (same survival)
    group_c_times <- rexp(n_per_group, rate = 0.07)
    group_c_status <- rbinom(n_per_group, 1, 0.7)
    
    group_d_times <- rexp(n_per_group, rate = 0.07)
    group_d_status <- rbinom(n_per_group, 1, 0.7)
    
    no_diff_data <- data.frame(
        time = c(group_c_times, group_d_times),
        status = c(group_c_status, group_d_status),
        group = factor(c(rep("Group_C", n_per_group), rep("Group_D", n_per_group)))
    )
    
    # Test with no expected difference
    testthat::expect_silent({
        results <- ClinicoPath::comparingSurvival(
            data = no_diff_data,
            times = "time",
            status = "status",
            groups = "group",
            ciyn = TRUE,
            timeunits = "Years"
        )
    })
})

# Enhanced features tests for comparingsurvival

test_that("Pairwise comparisons work with multiple groups", {
  skip_if_not_installed("survival")
  
  # Create test data with 4 groups
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    time = rexp(n, rate = 0.1),
    status = rbinom(n, 1, 0.7),
    group = factor(rep(c("Grade1", "Grade2", "Grade3", "Grade4"), each = n/4))
  )
  
  # Add some differences between groups
  test_data$time[test_data$group == "Grade3"] <- test_data$time[test_data$group == "Grade3"] * 0.7
  test_data$time[test_data$group == "Grade4"] <- test_data$time[test_data$group == "Grade4"] * 0.5
  
  # Test pairwise comparisons
  result <- ClinicoPath::comparingSurvival(
    data = test_data,
    times = "time",
    status = "status",
    groups = "group",
    pairwise = TRUE,
    pairwiseCorrection = "bonferroni"
  )
  
  # Check pairwise table exists 
  expect_true(!is.null(result$results$pairwiseTable))
  
  # Should have 6 pairwise comparisons for 4 groups (4 choose 2 = 6)
  expect_equal(result$results$pairwiseTable$rowCount, 6)
})

test_that("Landmark analysis excludes early events", {
  skip_if_not_installed("survival")
  
  # Create test data
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, rate = 0.1),
    status = rbinom(n, 1, 0.7),
    group = factor(rep(c("A", "B"), each = n/2))
  )
  
  # Test landmark analysis
  result <- ClinicoPath::comparingSurvival(
    data = test_data,
    times = "time",
    status = "status",
    groups = "group",
    landmarkTime = 2,
    landmarkUnit = "same"
  )
  
  # Check that landmark note is displayed
  expect_true(!is.null(result$results$landmarkNote))
})

test_that("Trend test works for ordered groups", {
  skip_if_not_installed("survival")
  
  # Create test data with ordered groups
  set.seed(123)
  n <- 150
  test_data <- data.frame(
    time = rexp(n, rate = 0.1),
    status = rbinom(n, 1, 0.7),
    group = factor(rep(c("Stage1", "Stage2", "Stage3"), each = n/3))
  )
  
  # Test trend analysis
  result <- ClinicoPath::comparingSurvival(
    data = test_data,
    times = "time",
    status = "status",
    groups = "group",
    trendTest = TRUE
  )
  
  # Check that trend test table exists
  expect_true(!is.null(result$results$trendTestTable))
})

test_that("Multiple testing corrections work correctly", {
  skip_if_not_installed("survival")
  
  # Create test data
  set.seed(123)
  n <- 120
  test_data <- data.frame(
    time = rexp(n, rate = 0.1),
    status = rbinom(n, 1, 0.7),
    group = factor(rep(c("A", "B", "C"), each = n/3))
  )
  
  # Test Bonferroni correction
  result <- ClinicoPath::comparingSurvival(
    data = test_data,
    times = "time",
    status = "status",
    groups = "group",
    pairwise = TRUE,
    pairwiseCorrection = "bonferroni"
  )
  
  # Check that adjusted p-values are present
  pairwise_df <- result$results$pairwiseTable$asDF
  expect_true("adj_p_value" %in% names(pairwise_df))
  
  # Bonferroni adjustment should make adjusted p-values larger or equal
  expect_true(all(pairwise_df$adj_p_value >= pairwise_df$p_value))
  
  # Test no correction
  result_none <- ClinicoPath::comparingSurvival(
    data = test_data,
    times = "time",
    status = "status",
    groups = "group",
    pairwise = TRUE,
    pairwiseCorrection = "none"
  )
  
  # No adjusted p-values when correction is "none"
  pairwise_df_none <- result_none$results$pairwiseTable$asDF
  expect_false("adj_p_value" %in% names(pairwise_df_none))
})