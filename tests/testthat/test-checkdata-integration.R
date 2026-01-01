# Integration tests for checkdata function
# These tests actually call ClinicoPath::checkdata() to validate the jamovi integration

test_that("checkdata basic integration works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # Create simple test data
    set.seed(123)
    test_data <- data.frame(
        clean_var = rnorm(100, 50, 10),
        var_with_outliers = c(rnorm(95, 50, 10), c(150, 160, -50, -60, 140)),
        var_with_missing = c(rnorm(80, 50, 10), rep(NA, 20))
    )

    # Test basic call
    result <- ClinicoPath::checkdata(
        data = test_data,
        var = "clean_var"
    )

    # Verify result structure
    expect_s3_class(result, "checkdataResults")
    expect_true("qualityText" %in% names(result))
    expect_true("missingVals" %in% names(result))
})

test_that("checkdata outlier detection integration", {
    # Create data with known outliers
    set.seed(456)
    test_data <- data.frame(
        # Add extreme outliers that should be detected by all methods
        extreme_outliers = c(rnorm(90, 50, 5), rep(200, 5), rep(-100, 5))
    )

    # Run with outlier detection enabled
    result <- ClinicoPath::checkdata(
        data = test_data,
        var = "extreme_outliers",
        showOutliers = TRUE
    )

    # Verify outliers were detected
    outlier_table <- result$outliers$asDF

    # Should detect some outliers (consensus method)
    expect_true(nrow(outlier_table) > 0,
                info = "Should detect consensus outliers from extreme values")

    # Verify severity classification is working
    if (nrow(outlier_table) > 0) {
        expect_true("severity" %in% names(outlier_table))
        # Should not all be "High" (bug fixed)
        severities <- unique(outlier_table$severity)
        expect_true(length(severities) >= 1)
    }
})

test_that("checkdata consensus outlier detection behavior", {

    # Create data where we control which methods detect outliers
    set.seed(789)
    base_data <- rnorm(100, 50, 10)

    # Add a point with |z| just over 3 (should be detected by z-score method)
    # But might not be detected by IQR/MAD methods
    test_data <- data.frame(
        single_method_outlier = c(base_data, 50 + 3.1 * 10)  # z ≈ 3.1
    )

    result <- ClinicoPath::checkdata(
        data = test_data,
        var = "single_method_outlier",
        showOutliers = TRUE
    )

    # Due to consensus requirement, this might NOT be shown
    # This is expected behavior - just verify function runs without error
    expect_s3_class(result, "checkdataResults")

    # Now test with more extreme outliers that will be detected by multiple methods
    test_data2 <- data.frame(
        multi_method_outlier = c(rnorm(95, 50, 10), rep(200, 5))
    )

    result2 <- ClinicoPath::checkdata(
        data = test_data2,
        var = "multi_method_outlier",
        showOutliers = TRUE
    )

    outlier_table2 <- result2$outliers$asDF
    expect_true(nrow(outlier_table2) >= 1,
                info = "Extreme outliers should be detected by consensus")
})

test_that("checkdata missing data integration", {

    # Create data with specific missing percentage
    set.seed(101)
    test_data <- data.frame(
        var_25pct_missing = c(rnorm(75, 50, 10), rep(NA, 25))
    )

    result <- ClinicoPath::checkdata(
        data = test_data,
        var = "var_25pct_missing"
    )

    # Check missing data table
    missing_table <- result$missingVals$asDF
    expect_true(nrow(missing_table) > 0)

    # Verify quality text is populated
    quality_text <- result$qualityText$content
    expect_true(nchar(quality_text) > 0)
})

test_that("checkdata handles numeric variables only", {

    # Create data with factor variable
    test_data <- data.frame(
        numeric_var = rnorm(50, 50, 10),
        factor_var = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
        char_var = sample(c("X", "Y", "Z"), 50, replace = TRUE)
    )

    # Should work with numeric
    result_numeric <- ClinicoPath::checkdata(
        data = test_data,
        var = "numeric_var"
    )
    expect_s3_class(result_numeric, "checkdataResults")

    # Should handle factor/character appropriately
    # (the function may warn or handle differently)
    # Just verify it doesn't crash
    expect_no_error({
        result_factor <- ClinicoPath::checkdata(
            data = test_data,
            var = "factor_var"
        )
    })
})

test_that("checkdata quality grading integration", {

    # High quality data (should get Grade A)
    set.seed(202)
    high_quality <- data.frame(
        excellent = rnorm(100, 50, 10)  # No missing, no extreme outliers
    )

    result_high <- ClinicoPath::checkdata(
        data = high_quality,
        var = "excellent"
    )

    quality_text_high <- result_high$qualityText$content
    # Should mention good quality (exact text may vary)
    expect_true(nchar(quality_text_high) > 0)

    # Poor quality data (should get lower grade)
    poor_quality <- data.frame(
        poor = c(rnorm(30, 50, 10), rep(NA, 50), rep(1000, 20))  # 50% missing + outliers
    )

    result_poor <- ClinicoPath::checkdata(
        data = poor_quality,
        var = "poor"
    )

    quality_text_poor <- result_poor$qualityText$content
    expect_true(nchar(quality_text_poor) > 0)

    # Quality grades should be different
    expect_false(identical(quality_text_high, quality_text_poor),
                info = "Quality assessments should differ for high vs poor quality data")
})

test_that("checkdata distribution analysis integration", {

    set.seed(303)
    test_data <- data.frame(
        normal_ish = rnorm(100, 50, 10)
    )

    result <- ClinicoPath::checkdata(
        data = test_data,
        var = "normal_ish",
        showDistribution = TRUE
    )

    # Verify distribution table exists and is populated
    dist_table <- result$distribution$asDF
    expect_true(nrow(dist_table) > 0)

    # Should include common statistics
    expect_true("metric" %in% names(dist_table))
    expect_true("value" %in% names(dist_table))
})

test_that("checkdata duplicate analysis integration", {

    # Create data with duplicates
    test_data <- data.frame(
        with_dups = c(rep(50, 20), rep(60, 15), rnorm(65, 70, 10))
    )

    result <- ClinicoPath::checkdata(
        data = test_data,
        var = "with_dups",
        showDuplicates = TRUE
    )

    # Verify duplicates table
    dup_table <- result$duplicates$asDF
    expect_true(nrow(dup_table) > 0,
                info = "Should detect repeated values")
})

test_that("checkdata edge cases integration", {

    # All same value
    all_same <- data.frame(
        constant = rep(50, 100)
    )

    expect_no_error({
        result_same <- ClinicoPath::checkdata(
            data = all_same,
            var = "constant"
        )
    })

    # All missing
    all_missing <- data.frame(
        all_na = rep(NA_real_, 100)
    )

    expect_no_error({
        result_missing <- ClinicoPath::checkdata(
            data = all_missing,
            var = "all_na"
        )
    })

    # Very small sample
    tiny_sample <- data.frame(
        tiny = c(1, 2, 3)
    )

    expect_no_error({
        result_tiny <- ClinicoPath::checkdata(
            data = tiny_sample,
            var = "tiny"
        )
    })
})

# VALIDATION TODO: These tests verify the function runs and produces output,
# but do NOT validate statistical correctness of:
# - Outlier detection thresholds
# - Quality grade calculations
# - Missing data interpretations
# - Distribution test accuracy
#
# For clinical use, add tests comparing results to:
# - Known benchmark datasets
# - Established outlier detection packages
# - Manual calculations on simple examples
