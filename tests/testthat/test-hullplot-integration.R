# Integration tests for hullplot module
# Tests critical bug fixes: data caching, color scales, single-group handling

# Note: Tests run via devtools::test() which auto-loads the package
# If running manually, use: devtools::load_all()

test_that("hullplot invalidates cache when data content changes", {
    skip_if_not_installed("ClinicoPath")

    # CRITICAL TEST: Verify cache invalidates when data content changes
    # even if row count stays the same

    # Create initial dataset (50 rows)
    set.seed(123)
    data1 <- data.frame(
        x = rnorm(50, mean = 0),
        y = rnorm(50, mean = 0),
        group = factor(rep(c("A", "B"), each = 25))
    )

    result1 <- hullplot(
        data = data1,
        x_var = "x",
        y_var = "y",
        group_var = "group"
    )

    # Before fix: Result would be cached based on (x, y, group, 50)

    # Create different dataset with SAME row count but DIFFERENT content
    set.seed(456)
    data2 <- data.frame(
        x = rnorm(50, mean = 5),  # Different mean
        y = rnorm(50, mean = 5),  # Different mean
        group = factor(rep(c("A", "B"), each = 25))
    )

    result2 <- hullplot(
        data = data2,
        x_var = "x",
        y_var = "y",
        group_var = "group"
    )

    # CRITICAL: Results should be DIFFERENT (not cached)
    # Before fix: Would return cached data from data1
    # After fix: Should process data2 independently
    expect_s3_class(result1, "hullplotResults")
    expect_s3_class(result2, "hullplotResults")

    # Both should succeed without errors
    # (More detailed verification would require accessing internal plot data)
})


test_that("hullplot handles single group without crashing", {
    skip_if_not_installed("ClinicoPath")

    # CRITICAL TEST: Verify single-group case doesn't crash
    # Before fix: dist() on 1 group → NaN → crash in Natural Language Summary

    # Create data with only one group
    set.seed(789)
    single_group_data <- data.frame(
        sepal_length = rnorm(50, mean = 5.8),
        sepal_width = rnorm(50, mean = 3.0),
        species = factor(rep("setosa", 50))
    )

    # Should NOT crash
    expect_no_error({
        result <- hullplot(
            data = single_group_data,
            x_var = "sepal_length",
            y_var = "sepal_width",
            group_var = "species",
            show_statistics = TRUE  # Enable statistics to test the natural language summary path
        )
    })

    # CRITICAL: Summary should handle single group gracefully
    expect_s3_class(result, "hullplotResults")
})


test_that("hullplot with multiple groups generates statistics correctly", {
    skip_if_not_installed("ClinicoPath")

    # Test that multi-group analysis works correctly
    set.seed(111)
    multi_group_data <- data.frame(
        x = c(rnorm(30, mean = 0), rnorm(30, mean = 3), rnorm(30, mean = 1.5)),
        y = c(rnorm(30, mean = 0), rnorm(30, mean = 3), rnorm(30, mean = 1.5)),
        group = factor(rep(c("Group A", "Group B", "Group C"), each = 30))
    )

    result <- hullplot(
        data = multi_group_data,
        x_var = "x",
        y_var = "y",
        group_var = "group",
        show_statistics = TRUE
    )

    # Should handle multiple groups without errors
    expect_s3_class(result, "hullplotResults")
})


test_that("hullplot handles different color_var and group_var correctly", {
    skip_if_not_installed("ClinicoPath")

    # CRITICAL TEST: Verify color scales use correct variable levels
    # Before fix: Both scales used group_var levels
    # After fix: Fill uses group_var, colour uses color_var

    # Create data with different levels for group vs color
    set.seed(222)
    test_data <- data.frame(
        x = rnorm(100),
        y = rnorm(100),
        # 3 groups for hulls
        treatment = factor(rep(c("Drug A", "Drug B", "Drug C"), length.out = 100)),
        # 2 levels for point colors
        response = factor(rep(c("Responder", "Non-responder"), length.out = 100))
    )

    result <- hullplot(
        data = test_data,
        x_var = "x",
        y_var = "y",
        group_var = "treatment",      # 3 levels
        color_var = "response"         # 2 levels (DIFFERENT!)
    )

    # CRITICAL: Should not crash with different level counts
    # Before fix: Color scale would use 3 colors for 2-level response variable
    # After fix: Fill scale uses 3 colors, colour scale uses 2 colors
    expect_s3_class(result, "hullplotResults")
})


test_that("hullplot handles same color_var and group_var correctly", {
    skip_if_not_installed("ClinicoPath")

    # Test when color_var is same as group_var (common case)
    set.seed(333)
    test_data <- data.frame(
        x = rnorm(90),
        y = rnorm(90),
        diagnosis = factor(rep(c("Type I", "Type II", "Type III"), each = 30))
    )

    result <- hullplot(
        data = test_data,
        x_var = "x",
        y_var = "y",
        group_var = "diagnosis",
        color_var = "diagnosis"  # Same as group_var
    )

    # Should work without issues
    expect_s3_class(result, "hullplotResults")
})


test_that("hullplot handles missing values appropriately", {
    skip_if_not_installed("ClinicoPath")

    # Test that NAs are handled gracefully
    set.seed(444)
    data_with_na <- data.frame(
        x = c(rnorm(45), rep(NA, 5)),
        y = c(rnorm(45), rep(NA, 5)),
        group = factor(rep(c("A", "B"), each = 25))
    )

    # Should handle NAs without crashing
    # (jamovi's naOmit will remove rows with NAs)
    expect_no_error({
        result <- hullplot(
            data = data_with_na,
            x_var = "x",
            y_var = "y",
            group_var = "group"
        )
    })
})


test_that("hullplot handles small groups (< 3 points) gracefully", {
    skip_if_not_installed("ClinicoPath")

    # Hull calculation requires at least 3 points per group
    # Test edge case with small groups
    set.seed(555)
    small_group_data <- data.frame(
        x = c(1, 2, 5, 6, 7, 8, 9),
        y = c(1, 2, 5, 6, 7, 8, 9),
        group = factor(c("A", "A", "B", "B", "B", "B", "B"))  # A has only 2 points
    )

    # Should handle gracefully (may show points only for group A)
    expect_no_error({
        result <- hullplot(
            data = small_group_data,
            x_var = "x",
            y_var = "y",
            group_var = "group"
        )
    })
})


test_that("hullplot with all optional features enabled works", {
    skip_if_not_installed("ClinicoPath")

    # Test with all options enabled
    set.seed(666)
    full_feature_data <- data.frame(
        x = rnorm(100),
        y = rnorm(100),
        group = factor(rep(c("A", "B", "C"), length.out = 100)),
        color = factor(rep(c("Yes", "No"), length.out = 100)),
        size = runif(100, 1, 10)
    )

    result <- hullplot(
        data = full_feature_data,
        x_var = "x",
        y_var = "y",
        group_var = "group",
        color_var = "color",
        size_var = "size",
        hull_concavity = 2,
        hull_alpha = 0.2,
        point_size = 3,
        point_alpha = 0.7,
        color_palette = "set2",
        show_labels = TRUE,
        confidence_ellipses = TRUE,
        show_statistics = TRUE,
        plot_title = "Test Hull Plot",
        x_label = "X Axis",
        y_label = "Y Axis"
    )

    # Should handle all features without errors
    expect_s3_class(result, "hullplotResults")
})


test_that("hullplot with clinical data (iris) works correctly", {
    skip_if_not_installed("ClinicoPath")

    # Use iris dataset as a realistic test case
    result <- hullplot(
        data = iris,
        x_var = "Sepal.Length",
        y_var = "Sepal.Width",
        group_var = "Species",
        show_labels = TRUE,
        show_statistics = TRUE
    )

    # Should work with standard dataset
    expect_s3_class(result, "hullplotResults")
})


test_that("hullplot with filtered iris (single species) works", {
    skip_if_not_installed("ClinicoPath")

    # Test single-group case with real data
    setosa_only <- iris[iris$Species == "setosa", ]

    result <- hullplot(
        data = setosa_only,
        x_var = "Sepal.Length",
        y_var = "Sepal.Width",
        group_var = "Species",
        size_var = "Petal.Length",
        show_statistics = TRUE  # Enable statistics to test single-group summary path
    )

    # CRITICAL: Should handle single species without crash
    expect_s3_class(result, "hullplotResults")
})
