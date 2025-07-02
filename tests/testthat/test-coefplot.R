test_that("coefplot works", {
    
    # Skip if required packages are not available
    skip_if_not_installed("coefplot")
    skip_if_not_installed("jtools")
    skip_if_not_installed("survival")
    
    # Create test data for different model types
    set.seed(42)
    n <- 200
    
    # Create comprehensive test dataset
    test_data <- data.frame(
        # Continuous outcome for linear regression
        continuous_outcome = rnorm(n, 50, 10),
        
        # Binary outcome for logistic regression
        binary_outcome = rbinom(n, 1, 0.3),
        
        # Count outcome for Poisson regression
        count_outcome = rpois(n, 3),
        
        # Time and event for Cox regression
        time_to_event = rexp(n, 0.1),
        event_occurred = rbinom(n, 1, 0.7),
        
        # Predictors
        age = rnorm(n, 65, 15),
        bmi = rnorm(n, 25, 5),
        treatment = factor(sample(c("A", "B"), n, replace = TRUE)),
        stage = factor(sample(c("I", "II", "III"), n, replace = TRUE)),
        continuous_biomarker = rnorm(n, 0, 1),
        
        # Categorical predictors
        sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
        smoking = factor(sample(c("Never", "Former", "Current"), n, replace = TRUE))
    )
    
    # Ensure realistic ranges
    test_data$age <- pmax(18, pmin(90, test_data$age))
    test_data$bmi <- pmax(15, pmin(45, test_data$bmi))
    test_data$time_to_event <- pmax(0.1, test_data$time_to_event)
    
    # Test 1: Linear regression
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "continuous_outcome",
            covs = c("age", "bmi", "treatment"),
            model_type = "linear",
            show_coefficient_plot = TRUE,
            show_model_summary = TRUE,
            show_coefficient_table = FALSE
        )
    })
    
    # Test 2: Logistic regression
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "binary_outcome",
            covs = c("age", "sex", "stage"),
            model_type = "logistic",
            ci_level = 0.95,
            show_coefficient_plot = TRUE,
            show_model_summary = TRUE
        )
    })
    
    # Test 3: Cox regression
    testthat::expect_no_error({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "event_occurred",
            time_var = "time_to_event",
            covs = c("age", "treatment", "continuous_biomarker"),
            model_type = "cox",
            show_coefficient_plot = TRUE,
            show_model_summary = TRUE,
            show_coefficient_table = TRUE
        )
    })
    
    # Test 4: Poisson regression
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "count_outcome",
            covs = c("age", "smoking"),
            model_type = "poisson",
            show_coefficient_plot = TRUE,
            show_model_summary = FALSE,
            show_coefficient_table = TRUE
        )
    })
    
    # Test 5: With intercept included
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "continuous_outcome",
            covs = c("age", "bmi"),
            model_type = "linear",
            include_intercept = TRUE,
            show_coefficient_plot = TRUE
        )
    })
    
    # Test 6: Different confidence levels
    ci_levels <- c(0.8, 0.9, 0.95, 0.99)
    
    for (ci in ci_levels) {
        testthat::expect_silent({
            results <- ClinicoPath::coefplot(
                data = test_data,
                dep = "binary_outcome",
                covs = c("age", "treatment"),
                model_type = "logistic",
                ci_level = ci,
                show_coefficient_plot = TRUE
            )
        })
    }
    
    # Test 7: Custom plotting options
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "continuous_outcome",
            covs = c("age", "bmi", "sex", "treatment"),
            model_type = "linear",
            point_size = 4,
            line_thickness = 1.5,
            custom_title = "Custom Regression Plot",
            custom_x_label = "Effect Size",
            show_coefficient_plot = TRUE
        )
    })
    
    # Test 8: Exponentiated coefficients
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "binary_outcome",
            covs = c("age", "bmi"),
            model_type = "logistic",
            exp_transform = TRUE,
            show_coefficient_plot = TRUE,
            show_coefficient_table = TRUE
        )
    })
    
    # Test 9: Multiple predictors
    testthat::expect_no_error({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "continuous_outcome",
            covs = c("age", "bmi", "sex", "treatment", "stage", "smoking"),
            model_type = "linear",
            show_coefficient_plot = TRUE,
            show_model_summary = TRUE,
            show_coefficient_table = TRUE
        )
    })
    
    # Test 10: Inner confidence intervals
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "binary_outcome",
            covs = c("age", "treatment", "stage"),
            model_type = "logistic",
            ci_level = 0.95,
            inner_ci_level = 0.8,
            show_coefficient_plot = TRUE
        )
    })
})

test_that("coefplot handles edge cases", {
    
    # Skip if required packages are not available
    skip_if_not_installed("coefplot")
    skip_if_not_installed("jtools")
    
    # Create minimal test data
    minimal_data <- data.frame(
        outcome = rnorm(20, 50, 10),
        predictor1 = rnorm(20, 0, 1),
        predictor2 = rnorm(20, 0, 1)
    )
    
    # Test 1: Empty variable selection should show instructions
    testthat::expect_no_error({
        results <- ClinicoPath::coefplot(
            data = minimal_data
        )
    })
    
    # Test 2: Only dependent variable selected
    testthat::expect_no_error({
        results <- ClinicoPath::coefplot(
            data = minimal_data,
            dep = "outcome"
        )
    })
    
    # Test 3: Minimal valid model
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = minimal_data,
            dep = "outcome",
            covs = "predictor1",
            model_type = "linear",
            show_coefficient_plot = TRUE
        )
    })
    
    # Test 4: Data with missing values
    missing_data <- data.frame(
        outcome = c(rnorm(15, 50, 10), rep(NA, 5)),
        predictor = c(rnorm(10, 0, 1), rep(NA, 5), rnorm(5, 0, 1)),
        group = factor(sample(c("A", "B"), 20, replace = TRUE))
    )
    
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = missing_data,
            dep = "outcome",
            covs = c("predictor", "group"),
            model_type = "linear",
            show_coefficient_plot = TRUE
        )
    })
    
    # Test 5: Very small dataset
    tiny_data <- data.frame(
        outcome = rnorm(10, 50, 10),
        predictor = rnorm(10, 0, 1)
    )
    
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = tiny_data,
            dep = "outcome",
            covs = "predictor",
            model_type = "linear",
            show_coefficient_plot = TRUE
        )
    })
})

test_that("coefplot model types work correctly", {
    
    # Skip if required packages are not available
    skip_if_not_installed("coefplot")
    skip_if_not_installed("jtools")
    skip_if_not_installed("survival")
    
    # Create specific test data for each model type
    set.seed(123)
    n <- 100
    
    # Test data for different scenarios
    model_test_data <- data.frame(
        # Linear regression outcome
        continuous_y = rnorm(n, 100, 20),
        
        # Binary outcome (ensure exactly 2 levels)
        binary_y = factor(sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7))),
        
        # Count outcome
        count_y = rpois(n, 2),
        
        # Survival data
        time = rexp(n, 0.05),
        status = rbinom(n, 1, 0.6),
        
        # Predictors
        age = rnorm(n, 60, 15),
        score = rnorm(n, 10, 3),
        category = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE))
    )
    
    # Ensure realistic ranges
    model_test_data$age <- pmax(20, pmin(90, model_test_data$age))
    model_test_data$time <- pmax(0.1, model_test_data$time)
    
    # Test linear regression
    testthat::expect_no_error({
        results <- ClinicoPath::coefplot(
            data = model_test_data,
            dep = "continuous_y",
            covs = c("age", "score", "category"),
            model_type = "linear",
            show_coefficient_plot = TRUE,
            show_model_summary = TRUE
        )
    })
    
    # Test logistic regression
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = model_test_data,
            dep = "binary_y",
            covs = c("age", "score"),
            model_type = "logistic",
            show_coefficient_plot = TRUE,
            show_coefficient_table = TRUE
        )
    })
    
    # Test Cox regression
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = model_test_data,
            dep = "status",
            time_var = "time",
            covs = c("age", "category"),
            model_type = "cox",
            show_coefficient_plot = TRUE,
            show_model_summary = TRUE
        )
    })
    
    # Test Poisson regression
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = model_test_data,
            dep = "count_y",
            covs = c("age", "score"),
            model_type = "poisson",
            show_coefficient_plot = TRUE,
            show_model_summary = TRUE
        )
    })
})

test_that("coefplot output options work", {
    
    # Skip if required packages are not available
    skip_if_not_installed("coefplot")
    skip_if_not_installed("jtools")
    
    # Create test data
    test_data <- data.frame(
        outcome = rnorm(50, 100, 20),
        predictor1 = rnorm(50, 0, 1),
        predictor2 = rnorm(50, 5, 2),
        group = factor(sample(c("A", "B", "C"), 50, replace = TRUE))
    )
    
    # Test plot only
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "outcome",
            covs = c("predictor1", "predictor2", "group"),
            model_type = "linear",
            show_coefficient_plot = TRUE,
            show_model_summary = FALSE,
            show_coefficient_table = FALSE
        )
    })
    
    # Test summary only
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "outcome",
            covs = c("predictor1", "predictor2"),
            model_type = "linear",
            show_coefficient_plot = FALSE,
            show_model_summary = TRUE,
            show_coefficient_table = FALSE
        )
    })
    
    # Test table only
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "outcome",
            covs = c("predictor1", "group"),
            model_type = "linear",
            show_coefficient_plot = FALSE,
            show_model_summary = FALSE,
            show_coefficient_table = TRUE
        )
    })
    
    # Test all outputs
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "outcome",
            covs = c("predictor1", "predictor2", "group"),
            model_type = "linear",
            show_coefficient_plot = TRUE,
            show_model_summary = TRUE,
            show_coefficient_table = TRUE
        )
    })
    
    # Test no outputs (should just run analysis)
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "outcome",
            covs = c("predictor1", "predictor2"),
            model_type = "linear",
            show_coefficient_plot = FALSE,
            show_model_summary = FALSE,
            show_coefficient_table = FALSE
        )
    })
})

test_that("coefplot parameter validation", {
    
    # Skip if required packages are not available
    skip_if_not_installed("coefplot")
    skip_if_not_installed("jtools")
    
    # Create standard test data
    test_data <- data.frame(
        outcome = rnorm(80, 100, 20),
        binary_outcome = factor(sample(c("Yes", "No"), 80, replace = TRUE)),
        predictor1 = rnorm(80, 0, 1),
        predictor2 = rnorm(80, 5, 2),
        group = factor(sample(c("A", "B"), 80, replace = TRUE))
    )
    
    # Test different point sizes
    point_sizes <- c(1, 2, 3, 5, 8)
    
    for (size in point_sizes) {
        testthat::expect_silent({
            results <- ClinicoPath::coefplot(
                data = test_data,
                dep = "outcome",
                covs = c("predictor1", "predictor2"),
                model_type = "linear",
                point_size = size,
                show_coefficient_plot = TRUE
            )
        })
    }
    
    # Test different confidence levels
    ci_levels <- c(0.8, 0.85, 0.9, 0.95, 0.99)
    
    for (ci in ci_levels) {
        testthat::expect_silent({
            results <- ClinicoPath::coefplot(
                data = test_data,
                dep = "binary_outcome",
                covs = c("predictor1", "group"),
                model_type = "logistic",
                ci_level = ci,
                show_coefficient_plot = TRUE
            )
        })
    }
    
    # Test coefficient selection options
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "outcome",
            covs = c("predictor1", "predictor2", "group"),
            model_type = "linear",
            coef_selection = "all",
            show_coefficient_plot = TRUE
        )
    })
    
    # Test with specific coefficients
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "outcome",
            covs = c("predictor1", "predictor2", "group"),
            model_type = "linear",
            coef_selection = "specific",
            specific_coefs = "predictor1, group",
            show_coefficient_plot = TRUE
        )
    })
    
    # Test excluding specific coefficients
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = test_data,
            dep = "outcome",
            covs = c("predictor1", "predictor2", "group"),
            model_type = "linear",
            coef_selection = "exclude",
            specific_coefs = "predictor2",
            show_coefficient_plot = TRUE
        )
    })
})

test_that("coefplot advanced features", {
    
    # Skip if required packages are not available
    skip_if_not_installed("coefplot")
    skip_if_not_installed("jtools")
    
    # Create comprehensive test data
    advanced_test_data <- data.frame(
        outcome = rnorm(120, 50, 15),
        binary_outcome = factor(sample(c("Success", "Failure"), 120, replace = TRUE, prob = c(0.4, 0.6))),
        age = rnorm(120, 65, 12),
        bmi = rnorm(120, 25, 4),
        biomarker = rnorm(120, 0, 1),
        treatment = factor(sample(c("Control", "Treatment"), 120, replace = TRUE)),
        stage = factor(sample(c("Early", "Advanced"), 120, replace = TRUE)),
        center = factor(sample(c("A", "B", "C"), 120, replace = TRUE))
    )
    
    # Test with custom labels
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = advanced_test_data,
            dep = "outcome",
            covs = c("age", "bmi", "treatment"),
            model_type = "linear",
            custom_title = "Effect of Predictors on Outcome",
            custom_x_label = "Regression Coefficient",
            show_coefficient_plot = TRUE
        )
    })
    
    # Test inner confidence intervals
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = advanced_test_data,
            dep = "binary_outcome",
            covs = c("age", "biomarker", "stage"),
            model_type = "logistic",
            ci_level = 0.95,
            inner_ci_level = 0.8,
            show_coefficient_plot = TRUE
        )
    })
    
    # Test exponentiated coefficients for linear model
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = advanced_test_data,
            dep = "outcome",
            covs = c("age", "treatment"),
            model_type = "linear",
            exp_transform = TRUE,
            show_coefficient_plot = TRUE,
            show_coefficient_table = TRUE
        )
    })
    
    # Test with many predictors
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = advanced_test_data,
            dep = "outcome",
            covs = c("age", "bmi", "biomarker", "treatment", "stage", "center"),
            model_type = "linear",
            show_coefficient_plot = TRUE,
            show_model_summary = TRUE,
            show_coefficient_table = TRUE
        )
    })
    
    # Test different line thickness
    testthat::expect_silent({
        results <- ClinicoPath::coefplot(
            data = advanced_test_data,
            dep = "binary_outcome",
            covs = c("age", "bmi", "treatment"),
            model_type = "logistic",
            line_thickness = 2,
            point_size = 4,
            show_coefficient_plot = TRUE
        )
    })
})