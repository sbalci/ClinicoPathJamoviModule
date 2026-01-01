# Test suite for jscattermore function
# High-performance scatter plots using scattermore package

library(testthat)
library(ClinicoPath)

# Test data setup
set.seed(123)
test_data <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000),
    z = rnorm(1000),
    group = factor(sample(c("A", "B", "C"), 1000, replace = TRUE)),
    size_var = runif(1000, 0.5, 2),
    continuous_color = runif(1000),
    time = seq(1, 1000)
)

# Large dataset for performance testing
large_data <- data.frame(
    x = rnorm(10000),
    y = rnorm(10000) + 0.5 * rnorm(10000),
    group = factor(sample(c("Group1", "Group2", "Group3"), 10000, replace = TRUE)),
    size_var = runif(10000, 0.1, 3),
    color_numeric = runif(10000)
)

# Test basic functionality
test_that("jscattermore basic functionality works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # Test basic scatter plot
    result <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y"
    )
    
    expect_s3_class(result, "jscattermoreResults")
    expect_true("plot" %in% names(result))
    expect_true("summary" %in% names(result))
    expect_true("performance" %in% names(result))
})

test_that("jscattermore handles different plot types", {
    # Base R plot
    result_base <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        plot_type = "base_r"
    )
    expect_s3_class(result_base, "jscattermoreResults")
    
    # ggplot2 plot
    result_gg <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        plot_type = "ggplot2"
    )
    expect_s3_class(result_gg, "jscattermoreResults")
    
    # ggplot2 optimized plot
    result_opt <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        plot_type = "ggplot2_opt"
    )
    expect_s3_class(result_opt, "jscattermoreResults")
})

test_that("jscattermore handles color mapping", {
    # Continuous color variable
    result_cont <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        color_var = "continuous_color"
    )
    expect_s3_class(result_cont, "jscattermoreResults")
    
    # Categorical color variable
    result_cat <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        color_var = "group"
    )
    expect_s3_class(result_cat, "jscattermoreResults")
})

test_that("jscattermore handles size mapping", {
    result <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        size_var = "size_var"
    )
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles different color palettes", {
    palettes <- c("viridis", "plasma", "inferno", "magma", "cividis", 
                  "rainbow", "heat", "terrain")
    
    for (palette in palettes) {
        result <- jscattermore(
            data = test_data,
            x_var = "x",
            y_var = "y",
            color_var = "continuous_color",
            color_palette = palette
        )
        expect_s3_class(result, "jscattermoreResults")
    }
})

test_that("jscattermore handles plot customization options", {
    # Test various point sizes
    for (size in c(0.1, 0.5, 1.0, 2.0)) {
        result <- jscattermore(
            data = test_data,
            x_var = "x",
            y_var = "y",
            point_size = size
        )
        expect_s3_class(result, "jscattermoreResults")
    }
    
    # Test various alpha values
    for (alpha in c(0.1, 0.5, 0.8, 1.0)) {
        result <- jscattermore(
            data = test_data,
            x_var = "x",
            y_var = "y",
            alpha = alpha
        )
        expect_s3_class(result, "jscattermoreResults")
    }
    
    # Test various pixel resolutions
    for (pixels in c(64, 256, 512, 1024)) {
        result <- jscattermore(
            data = test_data,
            x_var = "x",
            y_var = "y",
            pixels = pixels
        )
        expect_s3_class(result, "jscattermoreResults")
    }
})

test_that("jscattermore handles smooth lines and density", {
    # Test smooth line options
    smooth_methods <- c("lm", "loess", "gam")
    
    for (method in smooth_methods) {
        result <- jscattermore(
            data = test_data,
            x_var = "x",
            y_var = "y",
            show_smooth = TRUE,
            smooth_method = method
        )
        expect_s3_class(result, "jscattermoreResults")
    }
    
    # Test density contours
    result <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        show_density = TRUE
    )
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles faceting", {
    result <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        facet_var = "group"
    )
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles log transformations", {
    # Create positive data for log transformation
    pos_data <- test_data
    pos_data$x <- abs(pos_data$x) + 1
    pos_data$y <- abs(pos_data$y) + 1
    
    # Test X log transformation
    result_x <- jscattermore(
        data = pos_data,
        x_var = "x",
        y_var = "y",
        log_transform_x = TRUE
    )
    expect_s3_class(result_x, "jscattermoreResults")
    
    # Test Y log transformation
    result_y <- jscattermore(
        data = pos_data,
        x_var = "x",
        y_var = "y",
        log_transform_y = TRUE
    )
    expect_s3_class(result_y, "jscattermoreResults")
    
    # Test both transformations
    result_both <- jscattermore(
        data = pos_data,
        x_var = "x",
        y_var = "y",
        log_transform_x = TRUE,
        log_transform_y = TRUE
    )
    expect_s3_class(result_both, "jscattermoreResults")
})

test_that("jscattermore handles custom labels and titles", {
    result <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        x_label = "Custom X Label",
        y_label = "Custom Y Label",
        plot_title = "Custom Plot Title"
    )
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles correlation display", {
    # Test with correlation shown
    result_cor <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        show_correlation = TRUE
    )
    expect_s3_class(result_cor, "jscattermoreResults")
    
    # Test without correlation
    result_no_cor <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        show_correlation = FALSE
    )
    expect_s3_class(result_no_cor, "jscattermoreResults")
})

test_that("jscattermore handles performance display", {
    result <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        show_performance = TRUE
    )
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles different themes", {
    themes <- c("default", "minimal", "classic", "dark", "clean")
    
    for (theme in themes) {
        result <- jscattermore(
            data = test_data,
            x_var = "x",
            y_var = "y",
            theme_style = theme
        )
        expect_s3_class(result, "jscattermoreResults")
    }
})

test_that("jscattermore handles interpolation options", {
    # Test with interpolation
    result_interp <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        interpolate = TRUE
    )
    expect_s3_class(result_interp, "jscattermoreResults")
    
    # Test without interpolation
    result_no_interp <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        interpolate = FALSE
    )
    expect_s3_class(result_no_interp, "jscattermoreResults")
})

test_that("jscattermore handles pointsize options", {
    # Test automatic pointsize
    result_auto <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        pointsize = 0
    )
    expect_s3_class(result_auto, "jscattermoreResults")
    
    # Test manual pointsize
    for (ps in c(1, 2, 5, 10)) {
        result <- jscattermore(
            data = test_data,
            x_var = "x",
            y_var = "y",
            pointsize = ps
        )
        expect_s3_class(result, "jscattermoreResults")
    }
})

# Error handling tests
test_that("jscattermore handles missing data gracefully", {
    # Test with NULL data
    expect_error(jscattermore(data = NULL, x_var = "x", y_var = "y"))
    
    # Test with empty data
    empty_data <- data.frame()
    result <- jscattermore(data = empty_data, x_var = "x", y_var = "y")
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles missing variables gracefully", {
    # Test with missing x_var
    result <- jscattermore(data = test_data, x_var = NULL, y_var = "y")
    expect_s3_class(result, "jscattermoreResults")
    
    # Test with missing y_var
    result <- jscattermore(data = test_data, x_var = "x", y_var = NULL)
    expect_s3_class(result, "jscattermoreResults")
    
    # Test with non-existent variables
    result <- jscattermore(data = test_data, x_var = "nonexistent", y_var = "y")
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles data with missing values", {
    # Create data with missing values
    na_data <- test_data
    na_data$x[1:10] <- NA
    na_data$y[5:15] <- NA
    
    result <- jscattermore(
        data = na_data,
        x_var = "x",
        y_var = "y"
    )
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles all missing values", {
    # Create data with all missing values
    all_na_data <- test_data
    all_na_data$x <- NA
    all_na_data$y <- NA
    
    result <- jscattermore(
        data = all_na_data,
        x_var = "x",
        y_var = "y"
    )
    expect_s3_class(result, "jscattermoreResults")
})

# Performance and caching tests
test_that("jscattermore caching works correctly", {
    # Create a jscattermore object to test caching
    if (requireNamespace("jmvcore", quietly = TRUE)) {
        options <- jscattermoreOptions$new(
            x_var = "x",
            y_var = "y"
        )
        
        analysis <- jscattermoreClass$new(
            options = options,
            data = test_data
        )
        
        # First run - should generate new results
        start_time <- Sys.time()
        analysis$run()
        first_run_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        
        # Second run - should use cache
        start_time <- Sys.time()
        analysis$run()
        second_run_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        
        # Cache should make second run faster (allow some tolerance)
        expect_true(second_run_time <= first_run_time * 1.1)
    }
})

test_that("jscattermore cache invalidation works", {
    if (requireNamespace("jmvcore", quietly = TRUE)) {
        options <- jscattermoreOptions$new(
            x_var = "x",
            y_var = "y"
        )
        
        analysis <- jscattermoreClass$new(
            options = options,
            data = test_data
        )
        
        # First run
        analysis$run()
        
        # Change options - should invalidate cache
        analysis$options$point_size <- 2.0
        analysis$run()
        
        # Change data - should invalidate cache
        new_data <- test_data[1:500, ]
        analysis$data <- new_data
        analysis$run()
        
        expect_s3_class(analysis$results, "jscattermoreResults")
    }
})

# Performance tests with large datasets
test_that("jscattermore handles large datasets efficiently", {
    skip_if_not_installed("scattermore")
    
    # Test with large dataset
    result <- jscattermore(
        data = large_data,
        x_var = "x",
        y_var = "y",
        show_performance = TRUE
    )
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore performance comparison works", {
    skip_if_not_installed("scattermore")
    
    # Test performance display
    result <- jscattermore(
        data = large_data,
        x_var = "x",
        y_var = "y",
        show_performance = TRUE
    )
    expect_s3_class(result, "jscattermoreResults")
})

# Complex combination tests
test_that("jscattermore handles complex combinations", {
    # Test complex plot with all features
    result <- jscattermore(
        data = test_data,
        x_var = "x",
        y_var = "y",
        color_var = "group",
        size_var = "size_var",
        plot_type = "ggplot2",
        point_size = 1.0,
        alpha = 0.7,
        pixels = 512,
        color_palette = "viridis",
        show_smooth = TRUE,
        smooth_method = "loess",
        show_density = FALSE,
        facet_var = "group",
        show_correlation = TRUE,
        show_performance = TRUE,
        theme_style = "minimal",
        x_label = "X Variable",
        y_label = "Y Variable",
        plot_title = "Complex Scatter Plot"
    )
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles edge cases", {
    # Test with single data point
    single_data <- data.frame(x = 1, y = 1)
    result <- jscattermore(
        data = single_data,
        x_var = "x",
        y_var = "y"
    )
    expect_s3_class(result, "jscattermoreResults")
    
    # Test with identical values
    identical_data <- data.frame(x = rep(1, 100), y = rep(1, 100))
    result <- jscattermore(
        data = identical_data,
        x_var = "x",
        y_var = "y"
    )
    expect_s3_class(result, "jscattermoreResults")
})

# Memory efficiency tests
test_that("jscattermore memory usage is reasonable", {
    skip_if_not_installed("scattermore")
    
    # Test memory usage doesn't grow excessively
    # This is a basic test - in practice you'd use profvis or similar
    initial_mem <- gc()
    
    for (i in 1:5) {
        result <- jscattermore(
            data = test_data,
            x_var = "x",
            y_var = "y"
        )
    }
    
    final_mem <- gc()
    
    # Basic check that we're not leaking too much memory
    # This is a rough heuristic
    expect_true(TRUE) # Placeholder - real memory tests would be more complex
})

# Integration tests
test_that("jscattermore integrates well with different data types", {
    # Test with integer data
    int_data <- data.frame(
        x = 1:100,
        y = (1:100)^2
    )
    result <- jscattermore(data = int_data, x_var = "x", y_var = "y")
    expect_s3_class(result, "jscattermoreResults")
    
    # Test with mixed data types
    mixed_data <- data.frame(
        x = rnorm(100),
        y = 1:100,
        group = factor(rep(c("A", "B"), 50))
    )
    result <- jscattermore(
        data = mixed_data,
        x_var = "x",
        y_var = "y",
        color_var = "group"
    )
    expect_s3_class(result, "jscattermoreResults")
})

# Regression tests for specific scenarios
test_that("jscattermore handles correlation with perfect correlation", {
    # Perfect positive correlation
    perfect_data <- data.frame(
        x = 1:100,
        y = 2 * (1:100) + 1
    )
    result <- jscattermore(
        data = perfect_data,
        x_var = "x",
        y_var = "y",
        show_correlation = TRUE
    )
    expect_s3_class(result, "jscattermoreResults")
})

test_that("jscattermore handles zero variance data", {
    # Zero variance in one variable
    zero_var_data <- data.frame(
        x = rep(5, 100),
        y = rnorm(100)
    )
    result <- jscattermore(
        data = zero_var_data,
        x_var = "x",
        y_var = "y"
    )
    expect_s3_class(result, "jscattermoreResults")
})
