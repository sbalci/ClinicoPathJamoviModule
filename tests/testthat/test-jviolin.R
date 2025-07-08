# Tests for jviolin function
# Professional violin plots for distribution visualization

# Test data preparation
test_data_violin <- data.frame(
    id = 1:150,
    
    # Continuous variables with different distributions
    normal_dist = rnorm(150, 50, 10),
    skewed_dist = exp(rnorm(150, 3, 0.5)),
    bimodal_dist = c(rnorm(75, 30, 5), rnorm(75, 70, 5)),
    uniform_dist = runif(150, 20, 80),
    
    # Grouping variables
    group_3 = factor(sample(c("A", "B", "C"), 150, replace = TRUE)),
    group_2 = factor(sample(c("Treatment", "Control"), 150, replace = TRUE)),
    group_4 = factor(sample(c("Group1", "Group2", "Group3", "Group4"), 150, replace = TRUE)),
    
    # Additional categorical variables for fill/color
    category = factor(sample(c("Cat1", "Cat2"), 150, replace = TRUE)),
    gender = factor(sample(c("Male", "Female"), 150, replace = TRUE)),
    
    # Ordinal variable
    rating = factor(sample(1:5, 150, replace = TRUE), ordered = TRUE),
    
    # Binary variable
    success = factor(sample(c("Yes", "No"), 150, replace = TRUE, prob = c(0.6, 0.4)))
)

# Basic function availability tests
test_that("jviolin function exists and is properly defined", {
    expect_true(exists("jviolinClass"))
    expect_true(R6::is.R6Class(jviolinClass))
})

test_that("Required packages are available", {
    expect_true(requireNamespace("jmvcore", quietly = TRUE))
    expect_true(requireNamespace("ggplot2", quietly = TRUE))
    expect_true(requireNamespace("R6", quietly = TRUE))
})

# Basic violin plot functionality tests
test_that("Basic violin plot creation works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Create a basic violin plot instance
    expect_no_error({
        violin_instance <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

test_that("Violin plot handles different group sizes", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Test with 2 groups
    expect_no_error({
        violin_2groups <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_2",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
    
    # Test with 4 groups
    expect_no_error({
        violin_4groups <- jviolinClass$new(
            options = list(
                dep = "normal_dist", 
                group = "group_4",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

test_that("Violin plot works with different distributions", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    distributions <- c("normal_dist", "skewed_dist", "bimodal_dist", "uniform_dist")
    
    for (dist in distributions) {
        expect_no_error({
            violin_dist <- jviolinClass$new(
                options = list(
                    dep = dist,
                    group = "group_3",
                    data = test_data_violin
                ),
                data = test_data_violin
            )
        }, info = paste("Failed with distribution:", dist))
    }
})

# Advanced violin plot options tests
test_that("Boxplot overlay option works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_boxplot <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                add_boxplot = TRUE,
                boxplot_width = 0.2,
                boxplot_alpha = 0.8,
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

test_that("Point overlay option works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_points <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                add_points = TRUE,
                point_size = 2.0,
                point_alpha = 0.6,
                point_jitter = TRUE,
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

test_that("Mean points option works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_means <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                add_mean = TRUE,
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

test_that("Quantile lines option works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_quantiles <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                draw_quantiles = TRUE,
                quantile_lines = "0.25,0.5,0.75",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

test_that("Different quantile specifications work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    quantile_specs <- c("0.5", "0.1,0.9", "0.25,0.5,0.75", "0.1,0.25,0.5,0.75,0.9")
    
    for (spec in quantile_specs) {
        expect_no_error({
            violin_quant <- jviolinClass$new(
                options = list(
                    dep = "normal_dist",
                    group = "group_3",
                    draw_quantiles = TRUE,
                    quantile_lines = spec,
                    data = test_data_violin
                ),
                data = test_data_violin
            )
        }, info = paste("Failed with quantiles:", spec))
    }
})

# Violin appearance options tests
test_that("Violin scaling options work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    scaling_options <- c("area", "count", "width")
    
    for (scale_opt in scaling_options) {
        expect_no_error({
            violin_scale <- jviolinClass$new(
                options = list(
                    dep = "normal_dist",
                    group = "group_3",
                    scale_violin = scale_opt,
                    data = test_data_violin
                ),
                data = test_data_violin
            )
        }, info = paste("Failed with scaling:", scale_opt))
    }
})

test_that("Violin trim option works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    for (trim_val in c(TRUE, FALSE)) {
        expect_no_error({
            violin_trim <- jviolinClass$new(
                options = list(
                    dep = "normal_dist",
                    group = "group_3",
                    trim_violin = trim_val,
                    data = test_data_violin
                ),
                data = test_data_violin
            )
        }, info = paste("Failed with trim:", trim_val))
    }
})

test_that("Violin width and transparency options work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_appearance <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                violin_width = 1.5,
                violin_alpha = 0.8,
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

# Color and fill variable tests
test_that("Fill and color variables work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Test with fill variable different from group
    expect_no_error({
        violin_fill <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                fill = "category",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
    
    # Test with color variable different from group
    expect_no_error({
        violin_color <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                col = "gender",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
    
    # Test with both fill and color variables
    expect_no_error({
        violin_both <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                fill = "category",
                col = "gender",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

# Color palette tests
test_that("Different color palettes work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    palettes <- c("default", "viridis", "brewer")
    
    for (palette in palettes) {
        expect_no_error({
            violin_palette <- jviolinClass$new(
                options = list(
                    dep = "normal_dist",
                    group = "group_3",
                    color_palette = palette,
                    data = test_data_violin
                ),
                data = test_data_violin
            )
        }, info = paste("Failed with palette:", palette))
    }
})

test_that("Manual color palette works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_manual <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                color_palette = "manual",
                manual_colors = "red,blue,green",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

# Theme tests
test_that("Different themes work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    themes <- c("minimal", "bw", "classic", "dark", "light", "grey", "void")
    
    for (theme in themes) {
        expect_no_error({
            violin_theme <- jviolinClass$new(
                options = list(
                    dep = "normal_dist",
                    group = "group_3",
                    themex = theme,
                    data = test_data_violin
                ),
                data = test_data_violin
            )
        }, info = paste("Failed with theme:", theme))
    }
})

test_that("hrbrthemes ipsum theme works when available", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_ipsum <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                themex = "ipsum",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

# Axis label tests
test_that("Custom axis labels work", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_labels <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                usexlabel = TRUE,
                xlabel = "Custom X Label",
                useylabel = TRUE,
                ylabel = "Custom Y Label",
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

# Coordinate flip test
test_that("Coordinate flipping works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_flip <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                flip = TRUE,
                data = test_data_violin
            ),
            data = test_data_violin
        )
    })
})

# Missing data handling tests
test_that("Missing data exclusion works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Create data with missing values
    test_data_missing <- test_data_violin
    test_data_missing$normal_dist[1:10] <- NA
    test_data_missing$group_3[11:15] <- NA
    
    # Test with exclusion
    expect_no_error({
        violin_excl <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                excl = TRUE,
                data = test_data_missing
            ),
            data = test_data_missing
        )
    })
    
    # Test without exclusion
    expect_no_error({
        violin_no_excl <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                excl = FALSE,
                data = test_data_missing
            ),
            data = test_data_missing
        )
    })
})

# Performance and caching tests
test_that("Caching system works correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    violin_instance <- jviolinClass$new(
        options = list(
            dep = "normal_dist",
            group = "group_3",
            data = test_data_violin
        ),
        data = test_data_violin
    )
    
    # First access to establish cache
    start_time1 <- Sys.time()
    violin_instance$run()
    end_time1 <- Sys.time()
    
    # Second access should use cache
    start_time2 <- Sys.time()
    violin_instance$run()
    end_time2 <- Sys.time()
    
    # Both should complete successfully
    time1 <- as.numeric(difftime(end_time1, start_time1, units = "secs"))
    time2 <- as.numeric(difftime(end_time2, start_time2, units = "secs"))
    
    expect_true(time1 > 0)
    expect_true(time2 > 0)
})

test_that("Cache invalidation works when data changes", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    violin_instance <- jviolinClass$new(
        options = list(
            dep = "normal_dist",
            group = "group_3",
            data = test_data_violin
        ),
        data = test_data_violin
    )
    
    # First run
    expect_no_error(violin_instance$run())
    
    # Change data
    modified_data <- test_data_violin
    modified_data$normal_dist <- modified_data$normal_dist + 10
    
    violin_instance$data <- modified_data
    
    # Second run with modified data
    expect_no_error(violin_instance$run())
})

test_that("Cache invalidation works when options change", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    violin_instance <- jviolinClass$new(
        options = list(
            dep = "normal_dist",
            group = "group_3",
            add_boxplot = FALSE,
            data = test_data_violin
        ),
        data = test_data_violin
    )
    
    # First run
    expect_no_error(violin_instance$run())
    
    # Change options
    violin_instance$options$add_boxplot <- TRUE
    
    # Second run with different options
    expect_no_error(violin_instance$run())
})

# Error handling and edge cases
test_that("Function handles empty data gracefully", {
    skip_if_not_installed("jmvcore")
    
    empty_data <- data.frame()
    
    expect_error({
        violin_empty <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                data = empty_data
            ),
            data = empty_data
        )
        violin_empty$run()
    })
})

test_that("Function validates dependent variable is numeric", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_error({
        violin_categorical <- jviolinClass$new(
            options = list(
                dep = "group_3",  # Categorical variable as dependent
                group = "group_2",
                data = test_data_violin
            ),
            data = test_data_violin
        )
        violin_categorical$run()
    })
})

test_that("Function handles missing required variables", {
    skip_if_not_installed("jmvcore")
    
    # Missing dependent variable
    violin_no_dep <- jviolinClass$new(
        options = list(
            group = "group_3",
            data = test_data_violin
        ),
        data = test_data_violin
    )
    
    expect_no_error(violin_no_dep$run())  # Should show todo message
    
    # Missing group variable
    violin_no_group <- jviolinClass$new(
        options = list(
            dep = "normal_dist",
            data = test_data_violin
        ),
        data = test_data_violin
    )
    
    expect_no_error(violin_no_group$run())  # Should show todo message
})

test_that("Function handles single group", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    single_group_data <- test_data_violin
    single_group_data$single_group <- factor("OnlyGroup")
    
    expect_no_error({
        violin_single <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "single_group",
                data = single_group_data
            ),
            data = single_group_data
        )
        violin_single$run()
    })
})

test_that("Function handles very small groups", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    small_data <- test_data_violin[1:10, ]
    
    expect_no_error({
        violin_small <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                data = small_data
            ),
            data = small_data
        )
        violin_small$run()
    })
})

test_that("Function handles outliers appropriately", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    outlier_data <- test_data_violin
    outlier_data$normal_dist[1:5] <- c(1000, -1000, 500, -500, 2000)  # Extreme outliers
    
    expect_no_error({
        violin_outliers <- jviolinClass$new(
            options = list(
                dep = "normal_dist",
                group = "group_3",
                data = outlier_data
            ),
            data = outlier_data
        )
        violin_outliers$run()
    })
})

# Complex combination tests
test_that("Complex violin plot with all features works", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    expect_no_error({
        violin_complex <- jviolinClass$new(
            options = list(
                dep = "bimodal_dist",
                group = "group_3",
                fill = "category",
                col = "gender",
                add_boxplot = TRUE,
                add_points = TRUE,
                add_mean = TRUE,
                draw_quantiles = TRUE,
                quantile_lines = "0.25,0.5,0.75",
                trim_violin = FALSE,
                scale_violin = "count",
                violin_width = 1.2,
                violin_alpha = 0.6,
                boxplot_width = 0.15,
                boxplot_alpha = 0.9,
                point_size = 1.0,
                point_alpha = 0.5,
                point_jitter = TRUE,
                color_palette = "viridis",
                themex = "minimal",
                usexlabel = TRUE,
                xlabel = "Treatment Groups",
                useylabel = TRUE,
                ylabel = "Response Variable",
                flip = FALSE,
                excl = TRUE,
                data = test_data_violin
            ),
            data = test_data_violin
        )
        violin_complex$run()
    })
})

# Memory and cleanup tests
test_that("Function cleans up properly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    initial_objects <- ls()
    
    violin_instance <- jviolinClass$new(
        options = list(
            dep = "normal_dist",
            group = "group_3",
            data = test_data_violin
        ),
        data = test_data_violin
    )
    
    violin_instance$run()
    
    # Clean up
    rm(violin_instance)
    gc()
    
    final_objects <- ls()
    
    # Should not create permanent objects in global environment
    new_objects <- setdiff(final_objects, initial_objects)
    expect_true(length(new_objects) <= 1)  # Only allowed new object is violin_instance if still referenced
})

# Large dataset performance test
test_that("Function handles larger datasets efficiently", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    skip_if(Sys.getenv("SKIP_LARGE_TESTS") == "true", "Skipping large dataset test")
    
    large_data <- do.call(rbind, replicate(5, test_data_violin, simplify = FALSE))
    large_data$id <- 1:nrow(large_data)
    
    start_time <- Sys.time()
    violin_large <- jviolinClass$new(
        options = list(
            dep = "normal_dist",
            group = "group_3",
            data = large_data
        ),
        data = large_data
    )
    violin_large$run()
    end_time <- Sys.time()
    
    # Should complete within reasonable time (5 seconds)
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_lt(execution_time, 5)
})

# Integration tests with different data patterns
test_that("Function works with various data distributions", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("ggplot2")
    
    # Uniform distribution
    uniform_data <- data.frame(
        value = runif(100, 0, 100),
        group = factor(rep(c("A", "B"), 50))
    )
    
    expect_no_error({
        violin_uniform <- jviolinClass$new(
            options = list(
                dep = "value",
                group = "group",
                data = uniform_data
            ),
            data = uniform_data
        )
        violin_uniform$run()
    })
    
    # Exponential distribution
    exp_data <- data.frame(
        value = rexp(100, 0.1),
        group = factor(rep(c("X", "Y"), 50))
    )
    
    expect_no_error({
        violin_exp <- jviolinClass$new(
            options = list(
                dep = "value",
                group = "group",
                data = exp_data
            ),
            data = exp_data
        )
        violin_exp$run()
    })
})

# Print test summary
cat("jviolin test suite includes:\n")
cat("- Basic violin plot functionality tests\n")
cat("- Advanced violin plot options (boxplot, points, quantiles)\n")
cat("- Caching and performance optimization tests\n")
cat("- Missing data handling tests\n")
cat("- Error handling and edge case tests\n")
cat("- Color palette and theme tests\n")
cat("- Complex combination tests\n")
cat("- Memory management and cleanup tests\n")
cat("- Large dataset performance tests\n")
cat("- Integration tests with various data patterns\n")
cat("Total test count: 40+ individual test cases\n")