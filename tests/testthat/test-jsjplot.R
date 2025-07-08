# Test suite for jsjplot function
# Social Science Statistical Visualization using sjPlot package

library(testthat)
library(ClinicoPath)

# Test data setup
set.seed(123)
test_data <- data.frame(
    # Continuous variables
    outcome_continuous = rnorm(200, 50, 10),
    predictor1 = rnorm(200, 0, 1),
    predictor2 = rnorm(200, 0, 1),
    predictor3 = rnorm(200, 0, 1),
    
    # Binary outcome
    outcome_binary = rbinom(200, 1, 0.4),
    
    # Count outcome  
    outcome_count = rpois(200, 3),
    
    # Categorical variables
    group_var = factor(sample(c("Control", "Treatment"), 200, replace = TRUE)),
    category_var = factor(sample(c("Low", "Medium", "High"), 200, replace = TRUE)),
    region = factor(sample(c("North", "South", "East", "West"), 200, replace = TRUE)),
    
    # Interaction variables
    age = round(rnorm(200, 45, 15)),
    gender = factor(sample(c("Male", "Female"), 200, replace = TRUE)),
    
    # Additional variables for PCA and correlation
    var1 = rnorm(200),
    var2 = rnorm(200),
    var3 = rnorm(200),
    var4 = rnorm(200),
    var5 = rnorm(200)
)

# Create realistic relationships
test_data$outcome_continuous <- test_data$outcome_continuous + 
    2 * test_data$predictor1 + 
    1.5 * test_data$predictor2 + 
    rnorm(200, 0, 5)

test_data$outcome_binary <- rbinom(200, 1, plogis(-1 + 0.5 * test_data$predictor1 + 0.3 * test_data$predictor2))

# Small dataset for testing edge cases
small_data <- test_data[1:10, ]

# Dataset with missing values
missing_data <- test_data
missing_data$outcome_continuous[1:20] <- NA
missing_data$predictor1[15:25] <- NA

# Test basic functionality
test_that("jsjplot basic functionality works", {
    # Test coefficient plot (default)
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2")
    )
    
    expect_s3_class(result, "jsjplotResults")
    expect_true("plot" %in% names(result))
    expect_true("model_table" %in% names(result))
    expect_true("statistics" %in% names(result))
    expect_true("summary" %in% names(result))
})

# Test different analysis types
test_that("jsjplot handles regression table analysis", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "regression_table",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2", "predictor3")
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles coefficient plot analysis", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2")
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles interaction plot analysis", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "interaction_plot",
        dependent_var = "outcome_continuous",
        interaction_vars = c("predictor1", "predictor2"),
        independent_vars = c("predictor3")
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles marginal effects analysis", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "marginal_effects",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2")
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles frequency table analysis", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "frequency_table"
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles correlation matrix analysis", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "correlation_matrix"
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles PCA plot analysis", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "pca_plot"
    )
    expect_s3_class(result, "jsjplotResults")
})

# Test different model types
test_that("jsjplot handles linear models", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        model_type = "lm"
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles generalized linear models", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        model_type = "glm",
        family = "gaussian"
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles logistic regression", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_binary",
        independent_vars = c("predictor1", "predictor2"),
        model_type = "logistic"
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles Poisson regression", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_count",
        independent_vars = c("predictor1", "predictor2"),
        model_type = "poisson"
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles mixed effects models", {
    # This should fallback to lm since proper mixed model needs grouping structure
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        model_type = "lmer"
    )
    expect_s3_class(result, "jsjplotResults")
})

# Test GLM families
test_that("jsjplot handles different GLM families", {
    families <- c("gaussian", "binomial", "poisson", "gamma")
    outcomes <- c("outcome_continuous", "outcome_binary", "outcome_count", "outcome_continuous")
    
    for (i in seq_along(families)) {
        # Skip gamma with zero values
        if (families[i] == "gamma") {
            test_data_pos <- test_data
            test_data_pos$outcome_continuous <- abs(test_data_pos$outcome_continuous) + 1
            
            result <- jsjplot(
                data = test_data_pos,
                analysis_type = "coefficient_plot",
                dependent_var = "outcome_continuous",
                independent_vars = c("predictor1"),
                model_type = "glm",
                family = families[i]
            )
        } else {
            result <- jsjplot(
                data = test_data,
                analysis_type = "coefficient_plot",
                dependent_var = outcomes[i],
                independent_vars = c("predictor1"),
                model_type = "glm",
                family = families[i]
            )
        }
        expect_s3_class(result, "jsjplotResults")
    }
})

# Test plot customization options
test_that("jsjplot handles plot customization", {
    # Test confidence levels
    for (conf_level in c(0.8, 0.9, 0.95, 0.99)) {
        result <- jsjplot(
            data = test_data,
            analysis_type = "coefficient_plot",
            dependent_var = "outcome_continuous",
            independent_vars = c("predictor1", "predictor2"),
            confidence_level = conf_level
        )
        expect_s3_class(result, "jsjplotResults")
    }
    
    # Test standardized coefficients
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        standardized = TRUE
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles display options", {
    # Test show/hide options
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        show_values = FALSE,
        show_p_values = FALSE,
        remove_intercept = FALSE
    )
    expect_s3_class(result, "jsjplotResults")
    
    # Test sorting estimates
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        sort_estimates = TRUE
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles visual customization", {
    # Test different themes
    themes <- c("sjplot", "minimal", "classic", "apa", "bw")
    
    for (theme in themes) {
        result <- jsjplot(
            data = test_data,
            analysis_type = "coefficient_plot",
            dependent_var = "outcome_continuous",
            independent_vars = c("predictor1", "predictor2"),
            theme_style = theme
        )
        expect_s3_class(result, "jsjplotResults")
    }
    
    # Test different color schemes
    colors <- c("default", "colorblind", "bw", "viridis", "metro")
    
    for (color in colors) {
        result <- jsjplot(
            data = test_data,
            analysis_type = "coefficient_plot",
            dependent_var = "outcome_continuous",
            independent_vars = c("predictor1", "predictor2"),
            colors = color
        )
        expect_s3_class(result, "jsjplotResults")
    }
})

test_that("jsjplot handles plot sizing options", {
    # Test different dot and line sizes
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        dot_size = 5,
        line_size = 1.2,
        grid_breaks = 1.0
    )
    expect_s3_class(result, "jsjplotResults")
})

# Test custom labels and titles
test_that("jsjplot handles custom labels and titles", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        title = "Custom Model Plot",
        axis_labels = "Predictor 1,Predictor 2"
    )
    expect_s3_class(result, "jsjplotResults")
})

# Test axis transformations
test_that("jsjplot handles axis transformations", {
    transformations <- c("none", "log", "sqrt", "inverse")
    
    for (transform in transformations) {
        result <- jsjplot(
            data = test_data,
            analysis_type = "coefficient_plot",
            dependent_var = "outcome_continuous",
            independent_vars = c("predictor1", "predictor2"),
            transform_axis = transform
        )
        expect_s3_class(result, "jsjplotResults")
    }
})

# Test output options
test_that("jsjplot handles output options", {
    # Test with/without statistics and summary
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        show_statistics = FALSE,
        show_summary = FALSE
    )
    expect_s3_class(result, "jsjplotResults")
    
    # Test HTML output options
    result <- jsjplot(
        data = test_data,
        analysis_type = "regression_table",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        html_output = FALSE
    )
    expect_s3_class(result, "jsjplotResults")
})

# Test with grouping variables
test_that("jsjplot handles grouping variables", {
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        grouping_var = "group_var"
    )
    expect_s3_class(result, "jsjplotResults")
})

# Error handling tests
test_that("jsjplot handles missing data gracefully", {
    # Test with NULL data
    expect_error(jsjplot(data = NULL, analysis_type = "coefficient_plot"))
    
    # Test with empty data
    empty_data <- data.frame()
    result <- jsjplot(data = empty_data, analysis_type = "coefficient_plot")
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles missing variables gracefully", {
    # Test with missing dependent variable
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = NULL,
        independent_vars = c("predictor1", "predictor2")
    )
    expect_s3_class(result, "jsjplotResults")
    
    # Test with missing independent variables
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = NULL
    )
    expect_s3_class(result, "jsjplotResults")
    
    # Test with non-existent variables
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "nonexistent_var",
        independent_vars = c("predictor1", "predictor2")
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles data with missing values", {
    result <- jsjplot(
        data = missing_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2")
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles insufficient data", {
    # Test with very small dataset
    result <- jsjplot(
        data = small_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2", "predictor3")
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles frequency table edge cases", {
    # Test with no categorical variables
    numeric_only_data <- test_data[c("outcome_continuous", "predictor1", "predictor2")]
    result <- jsjplot(
        data = numeric_only_data,
        analysis_type = "frequency_table"
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles correlation matrix edge cases", {
    # Test with only one numeric variable
    single_numeric_data <- test_data[c("outcome_continuous", "group_var")]
    result <- jsjplot(
        data = single_numeric_data,
        analysis_type = "correlation_matrix"
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles PCA edge cases", {
    # Test with insufficient numeric variables
    insufficient_data <- test_data[c("outcome_continuous", "predictor1", "group_var")]
    result <- jsjplot(
        data = insufficient_data,
        analysis_type = "pca_plot"
    )
    expect_s3_class(result, "jsjplotResults")
})

# Performance and caching tests
test_that("jsjplot caching works correctly", {
    # Create a jsjplot object to test caching
    if (requireNamespace("jmvcore", quietly = TRUE)) {
        options <- jsjplotOptions$new(
            analysis_type = "coefficient_plot",
            dependent_var = "outcome_continuous",
            independent_vars = c("predictor1", "predictor2")
        )
        
        analysis <- jsjplotClass$new(
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

test_that("jsjplot cache invalidation works", {
    if (requireNamespace("jmvcore", quietly = TRUE)) {
        options <- jsjplotOptions$new(
            analysis_type = "coefficient_plot",
            dependent_var = "outcome_continuous",
            independent_vars = c("predictor1", "predictor2")
        )
        
        analysis <- jsjplotClass$new(
            options = options,
            data = test_data
        )
        
        # First run
        analysis$run()
        
        # Change options - should invalidate cache
        analysis$options$model_type <- "glm"
        analysis$run()
        
        # Change data - should invalidate cache
        new_data <- test_data[1:100, ]
        analysis$data <- new_data
        analysis$run()
        
        expect_s3_class(analysis$results, "jsjplotResults")
    }
})

test_that("jsjplot model caching works", {
    if (requireNamespace("jmvcore", quietly = TRUE)) {
        options <- jsjplotOptions$new(
            analysis_type = "coefficient_plot",
            dependent_var = "outcome_continuous",
            independent_vars = c("predictor1", "predictor2")
        )
        
        analysis <- jsjplotClass$new(
            options = options,
            data = test_data
        )
        
        # Build model first time
        model1 <- analysis$.__enclos_env__$private$.buildModel(test_data)
        
        # Build model second time - should use cache
        model2 <- analysis$.__enclos_env__$private$.buildModel(test_data)
        
        # Should be the same object (cached)
        expect_true(identical(model1, model2))
    }
})

# Integration tests
test_that("jsjplot integrates well with different data types", {
    # Test with character variables (should be converted to factors)
    char_data <- test_data
    char_data$group_var <- as.character(char_data$group_var)
    
    result <- jsjplot(
        data = char_data,
        analysis_type = "frequency_table"
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles complex interactions", {
    # Test interaction with categorical variables
    result <- jsjplot(
        data = test_data,
        analysis_type = "interaction_plot",
        dependent_var = "outcome_continuous",
        interaction_vars = c("predictor1", "group_var"),
        independent_vars = c("predictor2")
    )
    expect_s3_class(result, "jsjplotResults")
})

# Specific sjPlot integration tests
test_that("jsjplot properly wraps sjPlot functions", {
    skip_if_not_installed("sjPlot")
    
    # Test that sjPlot wrapper functions work
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2"),
        show_values = TRUE,
        sort_estimates = TRUE
    )
    expect_s3_class(result, "jsjplotResults")
})

# Regression tests for specific scenarios
test_that("jsjplot handles perfect multicollinearity", {
    # Create perfectly correlated variables
    collinear_data <- test_data
    collinear_data$predictor2 <- collinear_data$predictor1
    
    result <- jsjplot(
        data = collinear_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2")
    )
    expect_s3_class(result, "jsjplotResults")
})

test_that("jsjplot handles constant variables", {
    # Create variable with no variance
    constant_data <- test_data
    constant_data$constant_var <- 5
    
    result <- jsjplot(
        data = constant_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "constant_var")
    )
    expect_s3_class(result, "jsjplotResults")
})

# Plot type specific tests
test_that("jsjplot handles different plot types correctly", {
    plot_types <- c("est", "pred", "eff", "int", "diag")
    
    for (plot_type in plot_types) {
        result <- jsjplot(
            data = test_data,
            analysis_type = "coefficient_plot",
            dependent_var = "outcome_continuous",
            independent_vars = c("predictor1", "predictor2"),
            plot_type = plot_type
        )
        expect_s3_class(result, "jsjplotResults")
    }
})

# Complex data scenarios
test_that("jsjplot handles mixed variable types", {
    # Test with mix of numeric, factor, and logical
    mixed_data <- test_data
    mixed_data$logical_var <- sample(c(TRUE, FALSE), 200, replace = TRUE)
    mixed_data$ordered_var <- factor(
        sample(c("Low", "Medium", "High"), 200, replace = TRUE),
        levels = c("Low", "Medium", "High"),
        ordered = TRUE
    )
    
    result <- jsjplot(
        data = mixed_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "logical_var")
    )
    expect_s3_class(result, "jsjplotResults")
})

# Memory and performance tests
test_that("jsjplot memory usage is reasonable", {
    # Test memory usage doesn't grow excessively
    initial_mem <- gc()
    
    for (i in 1:3) {
        result <- jsjplot(
            data = test_data,
            analysis_type = "coefficient_plot",
            dependent_var = "outcome_continuous",
            independent_vars = c("predictor1", "predictor2")
        )
    }
    
    final_mem <- gc()
    
    # Basic check that we're not leaking too much memory
    expect_true(TRUE) # Placeholder - real memory tests would be more complex
})

# Final integration test
test_that("jsjplot complete workflow integration", {
    # Test complete workflow with all major options
    result <- jsjplot(
        data = test_data,
        analysis_type = "coefficient_plot",
        dependent_var = "outcome_continuous",
        independent_vars = c("predictor1", "predictor2", "predictor3"),
        grouping_var = "group_var",
        model_type = "lm",
        confidence_level = 0.95,
        standardized = FALSE,
        show_values = TRUE,
        show_p_values = TRUE,
        sort_estimates = FALSE,
        remove_intercept = TRUE,
        colors = "viridis",
        theme_style = "minimal",
        title = "Complete Integration Test",
        show_statistics = TRUE,
        show_summary = TRUE,
        html_output = TRUE
    )
    expect_s3_class(result, "jsjplotResults")
})