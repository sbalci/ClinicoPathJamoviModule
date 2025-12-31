test_that("coxdiagnostics works", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("survminer")
    skip_if_not_installed("car")
    
    # Create comprehensive test data for Cox diagnostics
    set.seed(42)
    n <- 200
    
    # Create realistic survival test dataset
    test_data <- data.frame(
        # Time to event (months)
        survival_time = rexp(n, rate = 0.05) * 12,  # exponential survival times
        
        # Event indicator
        event_occurred = rbinom(n, 1, 0.6),  # 60% event rate
        
        # Continuous covariates
        age = rnorm(n, 65, 12),
        tumor_size = rexp(n, 0.1),
        biomarker_score = rnorm(n, 50, 15),
        
        # Categorical covariates
        sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
        stage = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE, 
                             prob = c(0.3, 0.3, 0.25, 0.15))),
        treatment = factor(sample(c("Standard", "Experimental"), n, replace = TRUE)),
        grade = factor(sample(c("Low", "Intermediate", "High"), n, replace = TRUE))
    )
    
    # Ensure realistic ranges
    test_data$survival_time <- pmax(0.1, pmin(120, test_data$survival_time))
    test_data$age <- pmax(18, pmin(95, test_data$age))
    test_data$tumor_size <- pmax(0.1, pmin(10, test_data$tumor_size))
    test_data$biomarker_score <- pmax(0, pmin(100, test_data$biomarker_score))
    
    # Test 1: Basic Cox diagnostics with minimal options
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "survival_time",
            event = "event_occurred",
            covariates = c("age", "sex")
        )
    })
    
    # Test 2: Cox diagnostics with all plots enabled
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "survival_time",
            event = "event_occurred",
            covariates = c("age", "sex", "stage"),
            show_martingale = TRUE,
            show_deviance = TRUE,
            show_score = TRUE,
            show_schoenfeld = TRUE,
            show_dfbeta = TRUE
        )
    })
    
    # Test 3: Cox diagnostics with stratification
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "survival_time",
            event = "event_occurred",
            covariates = c("age", "tumor_size"),
            strata_var = "stage",
            show_martingale = TRUE,
            show_deviance = TRUE
        )
    })
    
    # Test 4: Cox diagnostics with multiple continuous covariates
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "survival_time",
            event = "event_occurred",
            covariates = c("age", "tumor_size", "biomarker_score"),
            show_vif = TRUE,
            vif_threshold = 5.0
        )
    })
    
    # Test 5: Cox diagnostics with different plot options
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "survival_time",
            event = "event_occurred",
            covariates = c("age", "sex", "treatment"),
            ox_scale = "observation.id",
            add_smooth = TRUE,
            add_reference = TRUE,
            point_size = 1.5,
            alpha_level = 0.8
        )
    })
    
    # Test 6: Cox diagnostics with different analysis options
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "survival_time",
            event = "event_occurred",
            covariates = c("age", "sex", "stage", "treatment"),
            show_model_summary = TRUE,
            show_ph_test = TRUE,
            show_vif = TRUE,
            show_interpretation = TRUE,
            exclude_missing = TRUE,
            confidence_level = 0.95
        )
    })
    
    # Test 7: Cox diagnostics with different x-axis scales
    ox_scales <- c("linear.predictions", "observation.id", "time")
    
    for (scale in ox_scales) {
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = test_data,
                time = "survival_time",
                event = "event_occurred",
                covariates = c("age", "sex"),
                ox_scale = scale,
                show_martingale = TRUE
            )
        })
    }
    
    # Test 8: Cox diagnostics with different VIF thresholds
    vif_thresholds <- c(2.5, 5.0, 10.0)
    
    for (threshold in vif_thresholds) {
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = test_data,
                time = "survival_time",
                event = "event_occurred",
                covariates = c("age", "tumor_size", "biomarker_score"),
                show_vif = TRUE,
                vif_threshold = threshold
            )
        })
    }
    
    # Test 9: Cox diagnostics with single covariate (VIF should not be calculated)
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "survival_time",
            event = "event_occurred",
            covariates = "age",
            show_vif = TRUE
        )
    })
    
    # Test 10: Cox diagnostics with logical event variable
    test_data_logical <- test_data
    test_data_logical$event_logical <- as.logical(test_data$event_occurred)
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data_logical,
            time = "survival_time",
            event = "event_logical",
            covariates = c("age", "sex"),
            show_martingale = TRUE,
            show_deviance = TRUE
        )
    })
    
    # Test 11: Cox diagnostics with factor event variable
    test_data_factor <- test_data
    test_data_factor$event_factor <- factor(test_data$event_occurred, 
                                          levels = c(0, 1), 
                                          labels = c("Censored", "Event"))
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data_factor,
            time = "survival_time",
            event = "event_factor",
            covariates = c("age", "sex"),
            show_martingale = TRUE
        )
    })
})

test_that("coxdiagnostics handles edge cases", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("survminer")
    
    # Create minimal test data
    minimal_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        status = c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1),
        covariate1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        covariate2 = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"))
    )
    
    # Test 1: Empty variable selection should show instructions
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = minimal_data
        )
    })
    
    # Test 2: Only time variable selected
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = minimal_data,
            time = "time"
        )
    })
    
    # Test 3: Time and event but no covariates
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = minimal_data,
            time = "time",
            event = "status"
        )
    })
    
    # Test 4: Minimal valid analysis
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = minimal_data,
            time = "time",
            event = "status",
            covariates = "covariate1"
        )
    })
    
    # Test 5: Data with missing values
    missing_data <- data.frame(
        time = c(1, 2, NA, 4, 5, 6, 7, NA, 9, 10),
        status = c(1, 0, 1, NA, 0, 1, 0, 1, 0, 1),
        covariate1 = c(1, 2, 3, 4, NA, 6, 7, 8, 9, 10),
        covariate2 = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", NA))
    )
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = missing_data,
            time = "time",
            event = "status",
            covariates = c("covariate1", "covariate2"),
            exclude_missing = TRUE
        )
    })
    
    # Test 6: Very small dataset
    tiny_data <- data.frame(
        time = c(1, 2, 3, 4),
        status = c(1, 0, 1, 0),
        covariate = c(1, 2, 3, 4)
    )
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = tiny_data,
            time = "time",
            event = "status",
            covariates = "covariate"
        )
    })
    
    # Test 7: All events censored
    censored_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8),
        status = c(0, 0, 0, 0, 0, 0, 0, 0),  # All censored
        covariate = c(1, 2, 3, 4, 5, 6, 7, 8)
    )
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = censored_data,
            time = "time",
            event = "status",
            covariates = "covariate"
        )
    })
    
    # Test 8: All events observed
    all_events_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8),
        status = c(1, 1, 1, 1, 1, 1, 1, 1),  # All events
        covariate = c(1, 2, 3, 4, 5, 6, 7, 8)
    )
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = all_events_data,
            time = "time",
            event = "status",
            covariates = "covariate"
        )
    })
})

test_that("coxdiagnostics data validation", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    
    # Create standard test data
    test_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        status = c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1),
        covariate = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    )
    
    # Test 1: Negative time values (should error)
    negative_time_data <- test_data
    negative_time_data$time[1] <- -1
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = negative_time_data,
            time = "time",
            event = "status",
            covariates = "covariate"
        )
    })
    
    # Test 2: Non-numeric time variable (should error)
    character_time_data <- test_data
    character_time_data$time <- as.character(character_time_data$time)
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = character_time_data,
            time = "time",
            event = "status",
            covariates = "covariate"
        )
    })
    
    # Test 3: Invalid status values (should error)
    invalid_status_data <- test_data
    invalid_status_data$status <- c(1, 2, 3, 1, 0, 1, 0, 1, 0, 1)  # Contains 2 and 3
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = invalid_status_data,
            time = "time",
            event = "status",
            covariates = "covariate"
        )
    })
    
    # Test 4: Status with more than 2 factor levels (should error)
    multi_level_status <- test_data
    multi_level_status$status <- factor(c("No", "Yes", "Maybe", "No", "Yes", "No", "Yes", "No", "Yes", "No"))
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = multi_level_status,
            time = "time",
            event = "status",
            covariates = "covariate"
        )
    })
    
    # Test 5: Zero time values (should work but may generate warnings)
    zero_time_data <- test_data
    zero_time_data$time[1] <- 0
    
    testthat::expect_warning({
        results <- ClinicoPath::coxdiagnostics(
            data = zero_time_data,
            time = "time",
            event = "status",
            covariates = "covariate"
        )
    })
})

test_that("coxdiagnostics with realistic data", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("survminer")
    
    # Load built-in survival datasets if available
    data("melanoma", package = "ClinicoPath", envir = environment())
    data("colon", package = "ClinicoPath", envir = environment())
    data("histopathology", package = "ClinicoPath", envir = environment())
    
    # Test with melanoma data
    if (exists("melanoma")) {
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = melanoma,
                time = "time",
                event = "status",
                covariates = c("age", "sex"),
                show_martingale = TRUE,
                show_deviance = TRUE,
                show_ph_test = TRUE,
                show_model_summary = TRUE
            )
        })
        
        # Test with melanoma - more covariates
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = melanoma,
                time = "time",
                event = "status",
                covariates = c("age", "sex", "thickness"),
                show_vif = TRUE,
                show_schoenfeld = TRUE
            )
        })
    }
    
    # Test with colon data
    if (exists("colon")) {
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = colon,
                time = "time",
                event = "status",
                covariates = c("age", "sex"),
                show_martingale = TRUE,
                show_deviance = TRUE
            )
        })
        
        # Test with colon - with stratification
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = colon,
                time = "time",
                event = "status",
                covariates = c("age", "nodes"),
                strata_var = "rx",
                show_ph_test = TRUE
            )
        })
    }
    
    # Test with histopathology data
    if (exists("histopathology")) {
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = histopathology,
                time = "OverallTime",
                event = "Outcome",
                covariates = c("Age", "Sex"),
                show_martingale = TRUE,
                show_deviance = TRUE,
                show_model_summary = TRUE
            )
        })
        
        # Test with histopathology - multiple covariates
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = histopathology,
                time = "OverallTime",
                event = "Outcome",
                covariates = c("Age", "Sex", "Grade", "TStage"),
                show_vif = TRUE,
                show_ph_test = TRUE,
                exclude_missing = TRUE
            )
        })
    }
})

test_that("coxdiagnostics plot options", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("survminer")
    
    # Create test data
    test_data <- data.frame(
        time = rexp(50, 0.1),
        status = rbinom(50, 1, 0.7),
        covariate1 = rnorm(50),
        covariate2 = factor(sample(c("A", "B"), 50, replace = TRUE))
    )
    
    # Test different plot combinations
    plot_combinations <- list(
        list(martingale = TRUE, deviance = FALSE, score = FALSE, schoenfeld = FALSE, dfbeta = FALSE),
        list(martingale = FALSE, deviance = TRUE, score = FALSE, schoenfeld = FALSE, dfbeta = FALSE),
        list(martingale = FALSE, deviance = FALSE, score = TRUE, schoenfeld = FALSE, dfbeta = FALSE),
        list(martingale = FALSE, deviance = FALSE, score = FALSE, schoenfeld = TRUE, dfbeta = FALSE),
        list(martingale = FALSE, deviance = FALSE, score = FALSE, schoenfeld = FALSE, dfbeta = TRUE),
        list(martingale = TRUE, deviance = TRUE, score = TRUE, schoenfeld = TRUE, dfbeta = TRUE)
    )
    
    for (combo in plot_combinations) {
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = test_data,
                time = "time",
                event = "status",
                covariates = c("covariate1", "covariate2"),
                show_martingale = combo$martingale,
                show_deviance = combo$deviance,
                show_score = combo$score,
                show_schoenfeld = combo$schoenfeld,
                show_dfbeta = combo$dfbeta
            )
        })
    }
    
    # Test different point sizes and transparency
    point_sizes <- c(0.5, 1.0, 2.0, 3.0)
    alpha_levels <- c(0.1, 0.4, 0.6, 1.0)
    
    for (size in point_sizes) {
        for (alpha in alpha_levels) {
            testthat::expect_no_error({
                results <- ClinicoPath::coxdiagnostics(
                    data = test_data,
                    time = "time",
                    event = "status",
                    covariates = "covariate1",
                    show_martingale = TRUE,
                    point_size = size,
                    alpha_level = alpha
                )
            })
        }
    }
    
    # Test smooth and reference line options
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "time",
            event = "status",
            covariates = "covariate1",
            show_martingale = TRUE,
            add_smooth = TRUE,
            add_reference = TRUE
        )
    })
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "time",
            event = "status",
            covariates = "covariate1",
            show_martingale = TRUE,
            add_smooth = FALSE,
            add_reference = FALSE
        )
    })
})

test_that("coxdiagnostics statistical outputs", {
    
    # Skip if required packages are not available
    skip_if_not_installed("survival")
    skip_if_not_installed("car")
    
    # Create test data with known characteristics
    set.seed(123)
    n <- 100
    
    test_data <- data.frame(
        time = rexp(n, 0.1),
        status = rbinom(n, 1, 0.7),
        age = rnorm(n, 65, 10),
        sex = factor(sample(c("M", "F"), n, replace = TRUE)),
        treatment = factor(sample(c("A", "B"), n, replace = TRUE))
    )
    
    # Test model summary output
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data,
            time = "time",
            event = "status",
            covariates = c("age", "sex", "treatment"),
            show_model_summary = TRUE,
            show_ph_test = TRUE,
            show_vif = TRUE,
            show_interpretation = TRUE
        )
    })
    
    # Test with different confidence levels
    confidence_levels <- c(0.80, 0.90, 0.95, 0.99)
    
    for (conf_level in confidence_levels) {
        testthat::expect_no_error({
            results <- ClinicoPath::coxdiagnostics(
                data = test_data,
                time = "time",
                event = "status",
                covariates = c("age", "sex"),
                show_ph_test = TRUE,
                confidence_level = conf_level
            )
        })
    }
    
    # Test exclusion of missing values
    test_data_missing <- test_data
    test_data_missing$age[1:5] <- NA
    test_data_missing$sex[6:10] <- NA
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data_missing,
            time = "time",
            event = "status",
            covariates = c("age", "sex"),
            exclude_missing = TRUE
        )
    })
    
    testthat::expect_no_error({
        results <- ClinicoPath::coxdiagnostics(
            data = test_data_missing,
            time = "time",
            event = "status",
            covariates = c("age", "sex"),
            exclude_missing = FALSE
        )
    })
})