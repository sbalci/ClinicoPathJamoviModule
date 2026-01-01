test_that("cisingle works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    
    # Load test data
    data("histopathology", package = "ClinicoPath")
    
    # Test 1: Basic functionality with single variable
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            ciWidth = 95,
            method = "t",
            showPlot = FALSE,
            showDiagnostics = FALSE
        )
    })
    
    # Test 2: Multiple variables
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "OverallTime", "MeasurementA"),
            ciWidth = 95,
            method = "t",
            showPlot = FALSE,
            showDiagnostics = FALSE
        )
    })
    
    # Test 3: Different CI methods
    ci_methods <- c("t", "bootstrap", "normal")
    
    for (method in ci_methods) {
        testthat::expect_silent({
            results <- ClinicoPath::ciSingle(
                data = histopathology,
                deps = "Age",
                ciWidth = 95,
                method = method,
                bootstrapSamples = 100,  # Use smaller sample for testing
                showPlot = FALSE,
                showDiagnostics = FALSE
            )
        })
    }
    
    # Test 4: Different confidence levels
    ci_levels <- c(80, 90, 95, 99)
    
    for (level in ci_levels) {
        testthat::expect_silent({
            results <- ClinicoPath::ciSingle(
                data = histopathology,
                deps = "Age",
                ciWidth = level,
                method = "t",
                showPlot = FALSE,
                showDiagnostics = FALSE
            )
        })
    }
    
    # Test 5: With split-by grouping
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "OverallTime"),
            splitBy = "Sex",
            ciWidth = 95,
            method = "t",
            showPlot = FALSE,
            showDiagnostics = FALSE
        )
    })
    
    # Test 6: With multiple grouping variables
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            splitBy = "Group",
            ciWidth = 95,
            method = "t",
            showPlot = FALSE,
            showDiagnostics = FALSE
        )
    })
    
    # Test 7: With diagnostics enabled
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "OverallTime"),
            ciWidth = 95,
            method = "t",
            showPlot = FALSE,
            showDiagnostics = TRUE
        )
    })
    
    # Test 8: With plotting enabled
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "OverallTime"),
            ciWidth = 95,
            method = "t",
            showPlot = TRUE,
            showDiagnostics = FALSE
        )
    })
    
    # Test 9: Bootstrap method with different sample sizes
    bootstrap_samples <- c(100, 500, 1000)
    
    for (samples in bootstrap_samples) {
        testthat::expect_silent({
            results <- ClinicoPath::ciSingle(
                data = histopathology,
                deps = "Age",
                ciWidth = 95,
                method = "bootstrap",
                bootstrapSamples = samples,
                showPlot = FALSE,
                showDiagnostics = FALSE
            )
        })
    }
    
    # Test 10: All measurement variables
    measurement_vars <- c("MeasurementA", "MeasurementB", "Measurement1", "Measurement2")
    
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = measurement_vars,
            ciWidth = 95,
            method = "t",
            showPlot = FALSE,
            showDiagnostics = TRUE
        )
    })
    
    # Test 11: Combined options test
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "OverallTime"),
            splitBy = "Sex",
            ciWidth = 90,
            method = "bootstrap",
            bootstrapSamples = 200,
            showPlot = TRUE,
            showDiagnostics = TRUE
        )
    })
})

test_that("cisingle handles edge cases", {
    
    # Load test data
    data("histopathology", package = "ClinicoPath")
    
    # Test 1: Empty variable list should not crash
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology
        )
    })
    
    # Test 2: Single observation groups (should handle gracefully)
    small_data <- histopathology[1:5, ]
    
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = small_data,
            deps = "Age",
            splitBy = "Sex"
        )
    })
    
    # Test 3: Missing data handling
    test_data <- histopathology[1:50, ]
    test_data$Age[1:10] <- NA
    test_data$OverallTime[11:20] <- NA
    
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = test_data,
            deps = c("Age", "OverallTime"),
            ciWidth = 95,
            method = "t"
        )
    })
    
    # Test 4: Constant variables (should handle appropriately)
    constant_data <- histopathology[1:20, ]
    constant_data$ConstantVar <- 5.0
    
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = constant_data,
            deps = "ConstantVar",
            ciWidth = 95,
            method = "t"
        )
    })
    
    # Test 5: Very small dataset
    tiny_data <- histopathology[1:3, ]
    
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = tiny_data,
            deps = "Age",
            ciWidth = 95,
            method = "t"
        )
    })
    
    # Test 6: Large confidence intervals (edge of parameter range)
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            ciWidth = 99.99,
            method = "t"
        )
    })
    
    # Test 7: Small confidence intervals
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            ciWidth = 50,
            method = "t"
        )
    })
    
    # Test 8: All variables with split
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "Grade", "TStage", "OverallTime"),
            splitBy = "Group",
            method = "normal"
        )
    })
})

test_that("cisingle method validation", {
    
    data("histopathology", package = "ClinicoPath")
    
    # Test each method produces results
    methods <- c("t", "bootstrap", "normal")
    
    for (method in methods) {
        testthat::expect_silent({
            results <- ClinicoPath::ciSingle(
                data = histopathology,
                deps = "Age",
                method = method,
                bootstrapSamples = 100  # Keep bootstrap fast for testing
            )
        })
    }
    
    # Test bootstrap with different sample sizes
    testthat::expect_silent({
        results1 <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            method = "bootstrap",
            bootstrapSamples = 100
        )
        
        results2 <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            method = "bootstrap",
            bootstrapSamples = 1000
        )
    })
    
    # Test that methods complete without error for various data types
    numeric_vars <- c("Age", "OverallTime", "MeasurementA", "Anti-X-intensity")
    
    for (var in numeric_vars) {
        for (method in methods) {
            testthat::expect_silent({
                results <- ClinicoPath::ciSingle(
                    data = histopathology,
                    deps = var,
                    method = method,
                    bootstrapSamples = if(method == "bootstrap") 100 else 1000
                )
            })
        }
    }
})

test_that("cisingle diagnostic features", {
    
    data("histopathology", package = "ClinicoPath")
    
    # Test normality diagnostics
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "OverallTime", "MeasurementA"),
            showDiagnostics = TRUE,
            method = "t"
        )
    })
    
    # Test diagnostics with split-by
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "OverallTime"),
            splitBy = "Sex",
            showDiagnostics = TRUE,
            method = "t"
        )
    })
    
    # Test diagnostics with bootstrap method
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            showDiagnostics = TRUE,
            method = "bootstrap",
            bootstrapSamples = 100
        )
    })
    
    # Test diagnostics with normal approximation
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            showDiagnostics = TRUE,
            method = "normal"
        )
    })
})

test_that("cisingle plotting functionality", {
    
    data("histopathology", package = "ClinicoPath")
    
    # Test basic plotting
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "OverallTime"),
            showPlot = TRUE,
            method = "t"
        )
    })
    
    # Test plotting with split-by groups
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            splitBy = "Sex",
            showPlot = TRUE,
            method = "t"
        )
    })
    
    # Test plotting with multiple variables and groups
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = c("Age", "OverallTime"),
            splitBy = "Group",
            showPlot = TRUE,
            method = "normal"
        )
    })
    
    # Test plotting with bootstrap method
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            showPlot = TRUE,
            method = "bootstrap",
            bootstrapSamples = 100
        )
    })
    
    # Test plotting with different confidence levels
    testthat::expect_silent({
        results <- ClinicoPath::ciSingle(
            data = histopathology,
            deps = "Age",
            showPlot = TRUE,
            ciWidth = 80,
            method = "t"
        )
    })
})

test_that("cisingle parameter ranges", {
    
    data("histopathology", package = "ClinicoPath")
    
    # Test confidence level boundaries
    ci_levels <- c(50, 75, 90, 95, 99, 99.99)
    
    for (level in ci_levels) {
        testthat::expect_silent({
            results <- ClinicoPath::ciSingle(
                data = histopathology,
                deps = "Age",
                ciWidth = level,
                method = "t"
            )
        })
    }
    
    # Test bootstrap sample boundaries
    bootstrap_samples <- c(100, 500, 1000, 5000, 10000)
    
    for (samples in bootstrap_samples) {
        testthat::expect_silent({
            results <- ClinicoPath::ciSingle(
                data = histopathology,
                deps = "Age",
                method = "bootstrap",
                bootstrapSamples = samples
            )
        })
    }
    
    # Test all boolean combinations
    boolean_options <- expand.grid(
        showPlot = c(TRUE, FALSE),
        showDiagnostics = c(TRUE, FALSE)
    )
    
    for (i in 1:nrow(boolean_options)) {
        testthat::expect_silent({
            results <- ClinicoPath::ciSingle(
                data = histopathology,
                deps = "Age",
                showPlot = boolean_options$showPlot[i],
                showDiagnostics = boolean_options$showDiagnostics[i],
                method = "t"
            )
        })
    }
})
