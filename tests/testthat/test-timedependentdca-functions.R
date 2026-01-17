test_that("timedependentdca runs with synthetic data", {
    skip_if_not_installed('jmvReadWrite')
    skip_if_not_installed('survival')
    devtools::load_all()
    if (!exists("timedependentdca")) {
        source("../../R/timedependentdca.h.R")
        source("../../R/timedependentdca.b.R")
    }
    
    set.seed(123)
    n <- 200
    data <- data.frame(
        time = rexp(n, rate = 0.5) * 365, # Scale to days
        predictor = rnorm(n),
        event = rbinom(n, 1, 0.7)
    )
    
    
    # Run analysis
    results <- timedependentdca(
        data = data,
        time = "time",
        event = "event",
        predictors = "predictor",
        time_points = "365, 730",
        threshold_range_min = 0.01,
        threshold_range_max = 0.99,
        plot_net_benefit = TRUE
    )

    print("Class of results:")
    print(class(results))
    print("Results object:")
    print(results)
    
    expect_true(inherits(results, "R6"))
    expect_true(inherits(results, "timedependentdcaResults"))
    
    # Check tables
    expect_true(!is.null(results$netBenefitTable))
    
    # Check if table is filled (this might require running the analysis logic which happens on access usually in jmv)
    # In testthat with jmv, usually the object is returned but results are populated on run() which jmv calls.
    # When calling the function directly `timedependentdca(...)`, it returns the results object with analysis run.
    
    # Verify table has content
    # Note: accessing .asDF might trigger the run if not already run
    nb_table <- results$netBenefitTable$asDF
    expect_true(nrow(nb_table) > 0)
    
    # Check columns
    print("Columns in nb_table:")
    print(names(nb_table))
    expect_true("threshold" %in% names(nb_table))
    # It might use the predictor name or 'model'
    expect_true("net_benefit" %in% names(nb_table))
})

test_that("timedependentdca handles errors gracefully", {
    skip_if_not_installed('jmvReadWrite')
    
    # Data with invalid time
    data_bad <- data.frame(
        time = c(-1, 0, 10),
        event = c(1, 1, 1),
        predictor = c(0.5, 0.5, 0.5)
    )
    
    results <- timedependentdca(
        data = data_bad,
        time = "time",
        event = "event",
        predictors = "predictor",
        time_points = "5"
    )
    
    # It shouldn't crash, but maybe produce a warning/notice in results
    # Check for error notice if possible, or just ensuring no R error was thrown above
})

test_that("timedependentdca runs with multiple predictors", {
    skip_if_not_installed('jmvReadWrite')
    skip_if_not_installed('survival')
    
    set.seed(123)
    n <- 200
    data <- data.frame(
        time = rexp(n, rate = 0.5) * 365,
        model1 = rnorm(n),
        model2 = rnorm(n) + 0.5,
        event = rbinom(n, 1, 0.7)
    )
    
    results <- timedependentdca(
        data = data,
        time = "time",
        event = "event",
        predictors = c("model1", "model2"),
        time_points = "365",
        plot_net_benefit = FALSE
    )
    
    nb_table <- results$netBenefitTable$asDF
    expect_true(nrow(nb_table) > 0)
    expect_true("model" %in% names(nb_table))
    expect_true(length(unique(nb_table$model)) >= 2)
})

test_that("timedependentdca calculates model comparisons with bootstrap", {
    skip_if_not_installed('jmvReadWrite')
    skip_if_not_installed('survival')
    library(survival)
    
    set.seed(42)
    n <- 100
    data <- data.frame(
        time = rexp(n, rate = 1/365),
        model1 = rnorm(n),
        model2 = rnorm(n) + 0.5,
        event = rbinom(n, 1, 0.6)
    )
    
    # Run analysis with bootstrap and comparison
    results <- timedependentdca(
        data = data,
        time = "time",
        event = "event",
        predictors = c("model1", "model2"),
        time_points = "365",
        use_bootstrap = TRUE,
        bootstrap_iterations = 100,
        plot_net_benefit = TRUE
    )
    
    # Check comparison table
    comp_table <- results$comparisonTable$asDF
    
    print("Comparison Table Head:")
    print(head(comp_table))
    
    expect_true(nrow(comp_table) > 0)
    expect_true("diff_nb" %in% names(comp_table))
    expect_true("p_value" %in% names(comp_table))
    expect_true("contrast" %in% names(comp_table))
    
    # Ensure values are populated (not all NA)
    expect_true(any(!is.na(comp_table$diff_nb)))
    
    # Check contrast is correct format
    expect_true(all(grepl("vs", comp_table$contrast)))
})
