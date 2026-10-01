# Test file for parallelplot function
library(testthat)

# Create test data
create_test_data <- function() {
    set.seed(123)
    data.frame(
        var1 = rnorm(100, 50, 10),
        var2 = rnorm(100, 100, 15),
        var3 = rnorm(100, 75, 12),
        var4 = rnorm(100, 30, 8),
        group = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
        non_numeric = factor(sample(c("X", "Y"), 100, replace = TRUE))
    )
}

test_that("parallelplot function exists and can be called", {
  skip_if_not_installed('jmvReadWrite')
    expect_true(exists("parallelplot"))
    expect_true(is.function(parallelplot))
})

test_that("parallelplot handles basic input correctly", {
    test_data <- create_test_data()
    
    # Test basic functionality
    result <- parallelplot(
        data = test_data,
        vars = c("var1", "var2", "var3"),
        scaling = "std"
    )
    
    expect_true(!is.null(result))
    expect_true("parallelplotResults" %in% class(result))
})

test_that("parallelplot handles grouping variable", {
    test_data <- create_test_data()
    
    result <- parallelplot(
        data = test_data,
        vars = c("var1", "var2", "var3"),
        group = "group",
        scaling = "std"
    )
    
    expect_true(!is.null(result))
})

test_that("parallelplot handles different scaling methods", {
    test_data <- create_test_data()
    
    scaling_methods <- c("std", "uniminmax", "minmax", "none")
    
    for (method in scaling_methods) {
        result <- parallelplot(
            data = test_data,
            vars = c("var1", "var2"),
            scaling = method
        )
        
        expect_true(!is.null(result), info = paste("Failed for scaling method:", method))
    }
})

test_that("parallelplot handles missing data options", {
    test_data <- create_test_data()
    # Add some missing values
    test_data$var1[1:5] <- NA
    
    # Test with missing data excluded
    result1 <- parallelplot(
        data = test_data,
        vars = c("var1", "var2"),
        showMissing = FALSE
    )
    
    expect_true(!is.null(result1))
    
    # Test with missing data included
    result2 <- parallelplot(
        data = test_data,
        vars = c("var1", "var2"),
        showMissing = TRUE
    )
    
    expect_true(!is.null(result2))
})

test_that("parallelplot validates input correctly", {
    test_data <- create_test_data()
    
    # Test error when no numeric variables provided
    expect_error(
        parallelplot(
            data = test_data,
            vars = c("non_numeric")
        ),
        "At least 2 numeric variables"
    )
    
    # Test error when only one numeric variable provided
    expect_error(
        parallelplot(
            data = test_data,
            vars = c("var1")
        ),
        "At least 2 numeric variables"
    )
})

test_that("parallelplot color palettes work", {
    test_data <- create_test_data()
    
    palettes <- c("default", "viridis", "set1", "clinical")
    
    for (palette in palettes) {
        result <- parallelplot(
            data = test_data,
            vars = c("var1", "var2"),
            group = "group",
            colorPalette = palette
        )
        
        expect_true(!is.null(result), info = paste("Failed for palette:", palette))
    }
})

test_that("parallelplot alpha parameter works", {
    test_data <- create_test_data()
    
    alpha_values <- c(0.1, 0.5, 0.9, 1.0)
    
    for (alpha in alpha_values) {
        result <- parallelplot(
            data = test_data,
            vars = c("var1", "var2"),
            alpha = alpha
        )
        
        expect_true(!is.null(result), info = paste("Failed for alpha:", alpha))
    }
})

test_that("parallelplot summary table is created correctly", {
    test_data <- create_test_data()
    
    result <- parallelplot(
        data = test_data,
        vars = c("var1", "var2", "var3")
    )
    
    # Check that summary table exists and has correct structure
    summary_table <- result$summary
    expect_true(!is.null(summary_table))
    
    # Convert to data frame to check content
    summary_df <- as.data.frame(summary_table)
    expect_equal(nrow(summary_df), 3)  # Should have 3 rows for 3 variables
    expect_true("variable" %in% names(summary_df))
    expect_true("n" %in% names(summary_df))
    expect_true("mean" %in% names(summary_df))
    expect_true("sd" %in% names(summary_df))
})

test_that("parallelplot handles empty data gracefully", {
    empty_data <- data.frame(
        var1 = numeric(0),
        var2 = numeric(0)
    )
    
    expect_error(
        parallelplot(
            data = empty_data,
            vars = c("var1", "var2")
        ),
        "No complete cases found"
    )
})

test_that("parallelplot instructions are shown when no variables selected", {
    # No data= here: with data and zero variables, jmvcore's init() select()s a
    # 0-column frame and fails ("invalid 'row.names' length") before .run().
    result <- parallelplot(
        vars = c()  # No variables
    )
    
    expect_true(!is.null(result))
    # Instructions should be shown when no variables are selected. The item
    # is visible by default, so also check its text, and that selecting
    # variables hides it.
    instructions <- result$instructions
    expect_true(instructions$visible)
    expect_match(instructions$content, "Welcome to Parallel Coordinates Plot",
                 fixed = TRUE)

    selected <- parallelplot(data = create_test_data(),
                             vars = c("var1", "var2"))
    expect_false(selected$instructions$visible)
})
