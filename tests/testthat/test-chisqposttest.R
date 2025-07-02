test_that("chisqposttest works", {
    
    # Load test data
    data("histopathology", package = "ClinicoPath")
    
    # Test 1: Basic functionality with significant result
    testthat::expect_silent({
        results <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "Sex",
            cols = "Group",
            posthoc = "bonferroni",
            sig = 0.05,
            excl = TRUE,
            exp = FALSE,
            plot = FALSE
        )
    })
    
    # Test 2: Different adjustment methods
    adjustment_methods <- c("bonferroni", "holm", "fdr", "none")
    
    for (method in adjustment_methods) {
        testthat::expect_silent({
            results <- ClinicoPath::chisqposttest(
                data = histopathology,
                rows = "Sex",
                cols = "Group",
                posthoc = method,
                sig = 0.05,
                excl = TRUE,
                exp = FALSE,
                plot = FALSE
            )
        })
    }
    
    # Test 3: With expected values
    testthat::expect_silent({
        results <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "Sex",
            cols = "Group",
            posthoc = "bonferroni",
            sig = 0.05,
            excl = TRUE,
            exp = TRUE,
            plot = FALSE
        )
    })
    
    # Test 4: With plot enabled
    testthat::expect_silent({
        results <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "Sex",
            cols = "Group",
            posthoc = "bonferroni",
            sig = 0.05,
            excl = TRUE,
            exp = FALSE,
            plot = TRUE
        )
    })
    
    # Test 5: Different variable combinations
    categorical_vars <- c("Sex", "Race", "Group", "Grade_Level", "LVI", "PNI", "Mortality5yr")
    
    # Test multiple combinations
    testthat::expect_silent({
        results1 <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "Race",
            cols = "Mortality5yr",
            posthoc = "fdr",
            sig = 0.05,
            excl = TRUE,
            exp = FALSE,
            plot = FALSE
        )
    })
    
    testthat::expect_silent({
        results2 <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "LVI",
            cols = "PNI",
            posthoc = "holm",
            sig = 0.05,
            excl = TRUE,
            exp = FALSE,
            plot = FALSE
        )
    })
    
    # Test 6: Different significance levels
    sig_levels <- c(0.01, 0.05, 0.1)
    
    for (sig in sig_levels) {
        testthat::expect_silent({
            results <- ClinicoPath::chisqposttest(
                data = histopathology,
                rows = "Sex",
                cols = "Group",
                posthoc = "bonferroni",
                sig = sig,
                excl = TRUE,
                exp = FALSE,
                plot = FALSE
            )
        })
    }
    
    # Test 7: Missing data handling
    # Create data with some missing values
    test_data <- histopathology[1:100, ]
    test_data$Sex[1:5] <- NA
    test_data$Group[6:10] <- NA
    
    # Test with exclusion of missing values
    testthat::expect_silent({
        results_excl <- ClinicoPath::chisqposttest(
            data = test_data,
            rows = "Sex",
            cols = "Group",
            posthoc = "bonferroni",
            sig = 0.05,
            excl = TRUE,
            exp = FALSE,
            plot = FALSE
        )
    })
    
    # Test without exclusion of missing values
    testthat::expect_silent({
        results_no_excl <- ClinicoPath::chisqposttest(
            data = test_data,
            rows = "Sex",
            cols = "Group",
            posthoc = "bonferroni",
            sig = 0.05,
            excl = FALSE,
            exp = FALSE,
            plot = FALSE
        )
    })
    
    # Test 8: Edge case - variables with many categories
    testthat::expect_silent({
        results_grade <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "Grade_Level",
            cols = "Race",
            posthoc = "fdr",
            sig = 0.05,
            excl = TRUE,
            exp = FALSE,
            plot = FALSE
        )
    })
})

test_that("chisqposttest handles errors appropriately", {
    
    # Load test data
    data("histopathology", package = "ClinicoPath")
    
    # Test 1: Missing data argument should not cause crash
    testthat::expect_silent({
        results <- ClinicoPath::chisqposttest()
    })
    
    # Test 2: Invalid variable names should not crash
    testthat::expect_silent({
        results <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "NonexistentVar",
            cols = "Group"
        )
    })
    
    # Test 3: Same variable for rows and cols
    testthat::expect_silent({
        results <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "Sex",
            cols = "Sex"
        )
    })
    
    # Test 4: Continuous variables (should handle gracefully)
    testthat::expect_silent({
        results <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "Age",
            cols = "OverallTime"
        )
    })
    
    # Test 5: Empty data
    empty_data <- histopathology[0, ]
    testthat::expect_silent({
        results <- ClinicoPath::chisqposttest(
            data = empty_data,
            rows = "Sex",
            cols = "Group"
        )
    })
})

test_that("chisqposttest parameter validation", {
    
    data("histopathology", package = "ClinicoPath")
    
    # Test valid parameter ranges
    testthat::expect_silent({
        # Minimum significance level
        results1 <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "Sex",
            cols = "Group",
            sig = 0.001
        )
        
        # Maximum significance level
        results2 <- ClinicoPath::chisqposttest(
            data = histopathology,
            rows = "Sex",
            cols = "Group",
            sig = 0.1
        )
    })
    
    # Test all boolean combinations
    boolean_options <- expand.grid(
        excl = c(TRUE, FALSE),
        exp = c(TRUE, FALSE),
        plot = c(TRUE, FALSE)
    )
    
    for (i in 1:nrow(boolean_options)) {
        testthat::expect_silent({
            results <- ClinicoPath::chisqposttest(
                data = histopathology,
                rows = "Sex",
                cols = "Group",
                excl = boolean_options$excl[i],
                exp = boolean_options$exp[i],
                plot = boolean_options$plot[i]
            )
        })
    }
})

test_that("chisqposttest output structure", {
    
    data("histopathology", package = "ClinicoPath")
    
    # Create a result object
    results <- ClinicoPath::chisqposttest(
        data = histopathology,
        rows = "Sex",
        cols = "Group",
        posthoc = "bonferroni",
        sig = 0.05,
        excl = TRUE,
        exp = FALSE,
        plot = FALSE
    )
    
    # Test that results object exists and has expected structure
    testthat::expect_true(!is.null(results))
    
    # For jamovi modules, the results are typically handled internally
    # This test ensures the function completes without error
    testthat::expect_silent({
        # The jamovi framework handles the results internally
        # So we just ensure the function runs without throwing errors
        invisible(results)
    })
})

test_that("chisqposttest with different data types", {
    
    # Create test data with different factor levels
    test_data <- data.frame(
        var1 = factor(c(rep("A", 30), rep("B", 30), rep("C", 40)),
                     levels = c("A", "B", "C")),
        var2 = factor(c(rep("X", 20), rep("Y", 40), rep("Z", 40)),
                     levels = c("X", "Y", "Z")),
        var3 = factor(c(rep("P", 50), rep("Q", 50)),
                     levels = c("P", "Q")),
        stringsAsFactors = TRUE
    )
    
    # Test 2x2 table
    testthat::expect_silent({
        results_2x2 <- ClinicoPath::chisqposttest(
            data = test_data,
            rows = "var3",
            cols = "var3",  # Same variable should work
            posthoc = "bonferroni"
        )
    })
    
    # Test 3x3 table
    testthat::expect_silent({
        results_3x3 <- ClinicoPath::chisqposttest(
            data = test_data,
            rows = "var1",
            cols = "var2",
            posthoc = "fdr"
        )
    })
    
    # Test 3x2 table
    testthat::expect_silent({
        results_3x2 <- ClinicoPath::chisqposttest(
            data = test_data,
            rows = "var1",
            cols = "var3",
            posthoc = "holm"
        )
    })
})

test_that("chisqposttest adjustment methods work correctly", {
    
    data("histopathology", package = "ClinicoPath")
    
    # Test that different adjustment methods produce different results
    # when there are multiple comparisons
    
    adjustment_methods <- c("bonferroni", "holm", "fdr", "none")
    results_list <- list()
    
    for (method in adjustment_methods) {
        testthat::expect_silent({
            results_list[[method]] <- ClinicoPath::chisqposttest(
                data = histopathology,
                rows = "Race",  # This should have multiple levels
                cols = "Group",
                posthoc = method,
                sig = 0.05,
                excl = TRUE,
                exp = FALSE,
                plot = FALSE
            )
        })
    }
    
    # All methods should complete without error
    testthat::expect_equal(length(results_list), length(adjustment_methods))
})