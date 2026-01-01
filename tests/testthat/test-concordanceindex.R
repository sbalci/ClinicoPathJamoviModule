# Tests for concordanceindex function

test_that("concordanceindex works with basic inputs", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    skip_on_cran()

    # Create test data
    set.seed(123)
    n <- 100
    test_data <- data.frame(
        time = rexp(n, rate = 0.1),
        event = rbinom(n, 1, 0.6),
        risk_score = rnorm(n)
    )

    # Run analysis
    results <- jmv::concordanceindex(
        data = test_data,
        time = "time",
        event = "event",
        predictor = "risk_score"
    )

    # Check that results object is created
    expect_s3_class(results, "concordanceindexResults")

    # Check that C-index table exists
    expect_true(!is.null(results$cindexSummary))

    # Check that C-index is between 0 and 1
    cindex_val <- results$cindexSummary$asDF()$cindex[1]
    expect_true(cindex_val >= 0 && cindex_val <= 1)
})

test_that("concordanceindex handles missing required inputs", {
    skip_on_cran()

    test_data <- data.frame(
        time = rexp(50, rate = 0.1),
        event = rbinom(50, 1, 0.6),
        risk_score = rnorm(50)
    )

    # Missing time variable
    expect_error(
        jmv::concordanceindex(
            data = test_data,
            time = NULL,
            event = "event",
            predictor = "risk_score"
        ),
        NA  # Should not error, just return with notice
    )
})

test_that("concordanceindex detects too few events", {
    skip_on_cran()

    # Create data with very few events
    test_data <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        event = c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0),  # Only 2 events
        risk_score = 1:10
    )

    results <- jmv::concordanceindex(
        data = test_data,
        time = "time",
        event = "event",
        predictor = "risk_score"
    )

    # Function should run but with error notice
    expect_s3_class(results, "concordanceindexResults")
})

test_that("concordanceindex handles numeric event coding", {
    skip_on_cran()

    # Test with 0/1 numeric events
    test_data <- data.frame(
        time = rexp(100, rate = 0.1),
        event = rbinom(100, 1, 0.6),  # Already 0/1
        risk_score = rnorm(100)
    )

    results <- jmv::concordanceindex(
        data = test_data,
        time = "time",
        event = "event",
        predictor = "risk_score"
    )

    expect_s3_class(results, "concordanceindexResults")
    cindex_val <- results$cindexSummary$asDF()$cindex[1]
    expect_true(cindex_val >= 0 && cindex_val <= 1)
})

test_that("concordanceindex calculates Somers D correctly", {
    skip_on_cran()

    test_data <- data.frame(
        time = rexp(100, rate = 0.1),
        event = rbinom(100, 1, 0.6),
        risk_score = rnorm(100)
    )

    results <- jmv::concordanceindex(
        data = test_data,
        time = "time",
        event = "event",
        predictor = "risk_score",
        somers_d = TRUE
    )

    # Check Somers D table exists
    expect_true(!is.null(results$somersD))

    # Somers D should be 2*(C-0.5)
    cindex_val <- results$cindexSummary$asDF()$cindex[1]
    somers_val <- results$somersD$asDF()$somers_d[1]

    expect_equal(somers_val, 2 * (cindex_val - 0.5), tolerance = 0.001)
})

test_that("concordanceindex model comparison works", {
    skip_on_cran()

    test_data <- data.frame(
        time = rexp(100, rate = 0.1),
        event = rbinom(100, 1, 0.6),
        risk1 = rnorm(100),
        risk2 = rnorm(100),
        risk3 = rnorm(100)
    )

    results <- jmv::concordanceindex(
        data = test_data,
        time = "time",
        event = "event",
        predictor = "risk1",
        compare_models = TRUE,
        additional_predictors = c("risk2", "risk3")
    )

    # Check model comparison table
    expect_true(!is.null(results$modelComparison))
    comp_table <- results$modelComparison$asDF()

    # Should have 3 models
    expect_equal(nrow(comp_table), 3)

    # All C-indices should be between 0 and 1
    expect_true(all(comp_table$cindex >= 0 & comp_table$cindex <= 1))
})

test_that("concordanceindex handles missing data correctly", {
    skip_on_cran()

    # Create data with missing values
    test_data <- data.frame(
        time = c(rexp(90, rate = 0.1), rep(NA, 10)),
        event = c(rbinom(95, 1, 0.6), rep(NA, 5)),
        risk_score = c(rnorm(85), rep(NA, 15))
    )

    results <- jmv::concordanceindex(
        data = test_data,
        time = "time",
        event = "event",
        predictor = "risk_score"
    )

    # Should handle missing data via complete-case analysis
    expect_s3_class(results, "concordanceindexResults")
})

test_that("concordanceindex restricted time option works", {
    skip_on_cran()

    test_data <- data.frame(
        time = c(1:50, 51:100) * 0.5,  # Some events > 20
        event = rbinom(100, 1, 0.6),
        risk_score = rnorm(100)
    )

    results <- jmv::concordanceindex(
        data = test_data,
        time = "time",
        event = "event",
        predictor = "risk_score",
        restricted_time = TRUE,
        max_time = 20
    )

    expect_s3_class(results, "concordanceindexResults")
})

test_that("concordanceindex confidence intervals option works", {
    skip_on_cran()

    test_data <- data.frame(
        time = rexp(100, rate = 0.1),
        event = rbinom(100, 1, 0.6),
        risk_score = rnorm(100)
    )

    # With asymptotic CI (faster)
    results <- jmv::concordanceindex(
        data = test_data,
        time = "time",
        event = "event",
        predictor = "risk_score",
        confidence_intervals = TRUE,
        ci_method = "asymptotic"
    )

    cindex_table <- results$cindexSummary$asDF()
    expect_false(is.na(cindex_table$cindex_ci_lower[1]))
    expect_false(is.na(cindex_table$cindex_ci_upper[1]))
})
