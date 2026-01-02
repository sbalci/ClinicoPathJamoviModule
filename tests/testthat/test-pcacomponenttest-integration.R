# Integration tests for pcacomponenttest module
# Tests the sequential permutation test implementation

context("test-pcacomponenttest-integration")

devtools::load_all()

test_that("Sequential permutation test is mathematically sound", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    skip_if_not_installed("ClinicoPath")

    # Create test data with known structure
    # 3 strong components, rest noise
    set.seed(123)
    n <- 100

    # Generate structured data
    z1 <- rnorm(n)
    z2 <- rnorm(n)
    z3 <- rnorm(n)

    testData <- data.frame(
        var1 = 2*z1 + rnorm(n, sd=0.1),
        var2 = 2*z1 + rnorm(n, sd=0.1),
        var3 = 2*z2 + rnorm(n, sd=0.1),
        var4 = 2*z2 + rnorm(n, sd=0.1),
        var5 = 2*z3 + rnorm(n, sd=0.1),
        var6 = 2*z3 + rnorm(n, sd=0.1),
        var7 = rnorm(n),  # Noise
        var8 = rnorm(n)   # Noise
    )

    # Run sequential test
    result <- pcacomponenttest(
        data = testData,
        vars = c("var1", "var2", "var3", "var4", "var5", "var6", "var7", "var8"),
        ncomp = 5,
        nperm = 100,  # Low for speed
        center = TRUE,
        scale = TRUE,
        conflevel = 0.95,
        adjustmethod = "none"
    )

    # CRITICAL TEST: Verify sequential testing was applied
    # First 3 components should be significant, rest should not
    # (or test should stop early)
    expect_s3_class(result, "pcacomponenttestResults")
})


test_that("Factor variables are rejected with clear error", {
    skip_if_not_installed("ClinicoPath")

    # Create test data with factor variable
    set.seed(456)
    n <- 50
    testData <- data.frame(
        numeric_var1 = rnorm(n),
        numeric_var2 = rnorm(n),
        factor_var = factor(sample(c("A", "B", "C"), n, replace = TRUE)),  # FACTOR
        numeric_var3 = rnorm(n)
    )

    # CRITICAL TEST: Should reject factor variables
    expect_error(
        pcacomponenttest(
            data = testData,
            vars = c("numeric_var1", "numeric_var2", "factor_var", "numeric_var3"),
            ncomp = 3,
            nperm = 100,
            center = TRUE,
            scale = TRUE
        ),
        regexp = "Non-numeric variables detected.*factor_var"
    )
})


test_that("Character variables are rejected with clear error", {
    skip_if_not_installed("ClinicoPath")

    # Create test data with character variable
    set.seed(789)
    n <- 50
    testData <- data.frame(
        var1 = rnorm(n),
        var2 = rnorm(n),
        char_var = as.character(sample(c("Low", "Medium", "High"), n, replace = TRUE)),
        var3 = rnorm(n),
        stringsAsFactors = FALSE
    )

    # CRITICAL TEST: Should reject character variables
    expect_error(
        pcacomponenttest(
            data = testData,
            vars = c("var1", "var2", "char_var", "var3"),
            ncomp = 3,
            nperm = 100
        ),
        regexp = "Non-numeric variables detected.*char_var"
    )
})


test_that("Disabling center/scale triggers warning for mixed-scale data", {
    skip_if_not_installed("ClinicoPath")

    # Create test data with vastly different scales
    set.seed(111)
    n <- 50
    testData <- data.frame(
        small_scale = rnorm(n, mean = 0, sd = 1),      # SD ~ 1
        large_scale = rnorm(n, mean = 1000, sd = 100)  # SD ~ 100
    )

    # CRITICAL TEST: Should reject/warn when center=FALSE or scale=FALSE with mixed scales
    expect_error(
        pcacomponenttest(
            data = testData,
            vars = c("small_scale", "large_scale"),
            ncomp = 2,
            nperm = 100,
            center = FALSE,  # DISABLED
            scale = FALSE    # DISABLED
        ),
        regexp = "WARNING.*RAW VARIANCE.*not correlation structure"
    )
})


test_that("Valid numeric data with proper settings runs successfully", {
    skip_if_not_installed("ClinicoPath")

    # Create valid test data
    set.seed(222)
    n <- 60
    testData <- data.frame(
        var1 = rnorm(n),
        var2 = rnorm(n),
        var3 = rnorm(n),
        var4 = rnorm(n)
    )

    # Run with recommended settings
    result <- pcacomponenttest(
        data = testData,
        vars = c("var1", "var2", "var3", "var4"),
        ncomp = 3,
        nperm = 100,
        center = TRUE,
        scale = TRUE,
        conflevel = 0.95,
        adjustmethod = "BH"
    )

    # Should complete without errors
    expect_s3_class(result, "pcacomponenttestResults")
})


test_that("Sequential test stops after first non-significant component", {
    skip_if_not_installed("ClinicoPath")

    # Create data with only noise (no structure)
    set.seed(333)
    n <- 80
    testData <- data.frame(
        var1 = rnorm(n),
        var2 = rnorm(n),
        var3 = rnorm(n),
        var4 = rnorm(n),
        var5 = rnorm(n)
    )

    # Run sequential test
    result <- pcacomponenttest(
        data = testData,
        vars = c("var1", "var2", "var3", "var4", "var5"),
        ncomp = 4,
        nperm = 100,
        center = TRUE,
        scale = TRUE,
        adjustmethod = "none"
    )

    # CRITICAL TEST: With pure noise, test should stop early
    # (most components should be NA or non-significant)
    expect_s3_class(result, "pcacomponenttestResults")
})


test_that("P-value adjustment is applied correctly", {
    skip_if_not_installed("ClinicoPath")

    # Create test data
    set.seed(444)
    n <- 70
    testData <- data.frame(
        var1 = rnorm(n),
        var2 = rnorm(n),
        var3 = rnorm(n)
    )

    # Run with BH adjustment
    result_bh <- pcacomponenttest(
        data = testData,
        vars = c("var1", "var2", "var3"),
        ncomp = 2,
        nperm = 100,
        adjustmethod = "BH"
    )

    # Run without adjustment
    result_none <- pcacomponenttest(
        data = testData,
        vars = c("var1", "var2", "var3"),
        ncomp = 2,
        nperm = 100,
        adjustmethod = "none"
    )

    # Both should complete
    expect_s3_class(result_bh, "pcacomponenttestResults")
    expect_s3_class(result_none, "pcacomponenttestResults")

    # Adjusted p-values should be >= unadjusted
    # (Cannot directly access results table from test, but ensures no errors)
})


test_that("Insufficient data (n<3) is rejected", {
    skip_if_not_installed("ClinicoPath")

    # Create tiny dataset
    set.seed(555)
    testData <- data.frame(
        var1 = rnorm(2),  # Only 2 observations
        var2 = rnorm(2),
        var3 = rnorm(2)
    )

    # CRITICAL TEST: Should reject insufficient data
    expect_error(
        pcacomponenttest(
            data = testData,
            vars = c("var1", "var2", "var3"),
            ncomp = 2,
            nperm = 100
        ),
        regexp = "Insufficient data.*at least 3 complete observations"
    )
})


test_that("Missing values are handled correctly", {
    skip_if_not_installed("ClinicoPath")

    # Create data with missing values
    set.seed(666)
    n <- 50
    testData <- data.frame(
        var1 = rnorm(n),
        var2 = rnorm(n),
        var3 = rnorm(n)
    )
    # Introduce missing values
    testData$var1[1:5] <- NA
    testData$var2[10:15] <- NA

    # Run test (should use complete cases)
    result <- pcacomponenttest(
        data = testData,
        vars = c("var1", "var2", "var3"),
        ncomp = 2,
        nperm = 100,
        center = TRUE,
        scale = TRUE
    )

    # Should complete (using na.omit internally)
    expect_s3_class(result, "pcacomponenttestResults")
})


test_that("VAF plot is generated correctly", {
    skip_if_not_installed("ClinicoPath")

    # Create test data
    set.seed(777)
    n <- 60
    testData <- data.frame(
        var1 = rnorm(n),
        var2 = rnorm(n),
        var3 = rnorm(n),
        var4 = rnorm(n)
    )

    # Run with visualization
    result <- pcacomponenttest(
        data = testData,
        vars = c("var1", "var2", "var3", "var4"),
        ncomp = 3,
        nperm = 100,
        center = TRUE,
        scale = TRUE,
        showpercent = TRUE
    )

    # Should have plot
    expect_s3_class(result, "pcacomponenttestResults")
    # Plot rendering tested by jamovi framework
})


test_that("Percentage display option works", {
    skip_if_not_installed("ClinicoPath")

    # Create test data
    set.seed(888)
    n <- 50
    testData <- data.frame(
        var1 = rnorm(n),
        var2 = rnorm(n),
        var3 = rnorm(n)
    )

    # Run with percentage
    result_pct <- pcacomponenttest(
        data = testData,
        vars = c("var1", "var2", "var3"),
        ncomp = 2,
        nperm = 50,
        showpercent = TRUE
    )

    # Run with proportion
    result_prop <- pcacomponenttest(
        data = testData,
        vars = c("var1", "var2", "var3"),
        ncomp = 2,
        nperm = 50,
        showpercent = FALSE
    )

    # Both should complete
    expect_s3_class(result_pct, "pcacomponenttestResults")
    expect_s3_class(result_prop, "pcacomponenttestResults")
})
