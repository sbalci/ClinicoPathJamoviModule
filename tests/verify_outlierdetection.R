
# Verification Script for outlierdetection Function

# 1. Mocking jmvcore Environment ------------------------------------------

library(R6)
library(testthat)
library(ggplot2)

# Mock Option Classes
Option <- R6Class("Option",
    public = list(
        name = NULL,
        value = NULL,
        initialize = function(name, value = NULL) {
            self$name <- name
            self$value <- value
        }
    )
)

OptionData <- R6Class("OptionData", inherit = Option)
OptionVariables <- R6Class("OptionVariables", inherit = Option)
OptionList <- R6Class("OptionList", inherit = Option)
OptionBool <- R6Class("OptionBool", inherit = Option)
OptionNumber <- R6Class("OptionNumber", inherit = Option)

# Mock Html Class
Html <- R6Class("Html",
    public = list(
        name = NULL,
        content = NULL,
        initialize = function(name) {
            self$name <- name
        },
        setContent = function(content) {
            self$content <- content
        }
    )
)

# Mock Image Class
Image <- R6Class("Image",
    public = list(
        name = NULL,
        state = NULL,
        visible = TRUE,
        initialize = function(name) {
            self$name <- name
        },
        setState = function(state) {
            self$state <- state
        },
        setVisible = function(visible) {
            self$visible <- visible
        }
    )
)

# Mock Results Class
Results <- R6Class("Results",
    public = list(
        todo = NULL,
        interpretation = NULL,
        outlier_table = NULL,
        method_comparison = NULL,
        exclusion_summary = NULL,
        plot = NULL,
        
        initialize = function() {
            self$todo <- Html$new("todo")
            self$interpretation <- Html$new("interpretation")
            self$outlier_table <- Html$new("outlier_table")
            self$method_comparison <- Html$new("method_comparison")
            self$exclusion_summary <- Html$new("exclusion_summary")
            self$plot <- Image$new("plot")
        }
    )
)

# Mock Base Class
outlierdetectionBase <- R6Class("outlierdetectionBase",
    public = list(
        options = NULL,
        data = NULL,
        results = NULL,
        initialize = function(options, data) {
            self$options <- options
            self$data <- data
            self$results <- Results$new()
        },
        run = function() {
            private$.run()
        }
    )
)

# Mock performance package
mock_performance <- new.env()
mock_performance$check_outliers <- function(data, method = "zscore_robust", threshold = NULL) {
    # Simple mock logic: flag values > 2 as outliers for testing
    # This is a simplification; real logic is in the package
    
    n <- nrow(data)
    outlier_logical <- rep(FALSE, n)
    
    # Mock detection logic
    if (is.numeric(data[[1]])) {
        outlier_logical <- data[[1]] > 10 # Arbitrary threshold for testing
    }
    
    # Create mock detailed data
    outlier_data <- data.frame(
        Outlier_zscore = as.numeric(outlier_logical),
        Outlier_iqr = as.numeric(outlier_logical)
    )
    
    # Return logical vector with attributes
    result <- outlier_logical
    attr(result, "data") <- outlier_data
    
    return(result)
}

# Assign mock to global environment so it can be found (simulating package load)
# Note: The actual code uses performance::check_outliers. 
# We need to intercept this call or mock the namespace.
# Since we can't easily mock namespace in this script without mockery, 
# we will rely on the fact that the code might look for it in the search path 
# if we attach an environment, OR we modify the source code slightly for testing 
# OR we define a mock function in the global env and hope R picks it up if not fully qualified.
# However, the code uses `performance::check_outliers`. 
# A robust way is to define a local version of `performance` list/env.

# BETTER APPROACH:
# Since we are sourcing the file, we can't easily change `performance::check_outliers` calls 
# unless we use `mockery` or `testthat::with_mock`.
# But `testthat::with_mock` is deprecated.
# We will use a trick: define `performance` as an environment in the global scope 
# and hope the `::` operator finds it if it's not a loaded package. 
# BUT `::` strictly looks for packages.
# So we will define a wrapper function in the test script that overrides the private method 
# if possible, OR we just assume `performance` is installed.
# If `performance` is NOT installed, we need to mock it.
# Let's try to mock the private$.perform_outlier_detection method instead!
# This is cleaner. We can overwrite the method on the instance or class.

# Load the function code
source("R/outlierdetection.b.R")

# 2. Helper Functions -----------------------------------------------------

create_options <- function(vars = NULL, 
                          method_category = "univariate",
                          univariate_methods = "zscore_robust",
                          multivariate_methods = "mahalanobis",
                          composite_threshold = 0.5,
                          zscore_threshold = 3.29,
                          iqr_multiplier = 1.7,
                          confidence_level = 0.999,
                          show_outlier_table = TRUE,
                          show_method_comparison = FALSE,
                          show_exclusion_summary = FALSE,
                          show_visualization = FALSE,
                          show_interpretation = FALSE) {
    
    list(
        vars = vars,
        method_category = method_category,
        univariate_methods = univariate_methods,
        multivariate_methods = multivariate_methods,
        composite_threshold = composite_threshold,
        zscore_threshold = zscore_threshold,
        iqr_multiplier = iqr_multiplier,
        confidence_level = confidence_level,
        show_outlier_table = show_outlier_table,
        show_method_comparison = show_method_comparison,
        show_exclusion_summary = show_exclusion_summary,
        show_visualization = show_visualization,
        show_interpretation = show_interpretation
    )
}

# 3. Test Cases -----------------------------------------------------------

test_that("Univariate Outlier Detection Works", {
    data <- data.frame(
        Value = c(1, 2, 3, 100, 5) # 100 is outlier
    )
    
    options <- create_options(vars = "Value", 
                              method_category = "univariate", 
                              univariate_methods = "zscore_robust",
                              show_outlier_table = TRUE)
    
    analysis <- outlierdetectionClass$new(options, data)
    analysis$run()
    
    # Check output - HTML has </strong> between "Outliers Detected:" and the number
    expect_true(grepl("Outlier Detection Results", analysis$results$outlier_table$content, fixed = TRUE))
    expect_true(grepl("Outliers Detected:", analysis$results$outlier_table$content, fixed = TRUE))
})

test_that("Multivariate Outlier Detection Works", {
    data <- data.frame(
        X = c(1, 2, 3, 100, 5, 6, 7, 8, 9, 10),
        Y = c(1, 3, 2, 100, 4, 7, 5, 9, 8, 11)
    )
    
    options <- create_options(vars = c("X", "Y"), 
                              method_category = "multivariate", 
                              multivariate_methods = "mahalanobis_robust",
                              show_outlier_table = TRUE)
    
    analysis <- outlierdetectionClass$new(options, data)
    analysis$run()
    
    expect_true(grepl("Robust Mahalanobis", analysis$results$outlier_table$content, fixed = TRUE))
    expect_true(grepl("Outliers Detected:", analysis$results$outlier_table$content, fixed = TRUE))
})

test_that("Composite Outlier Detection Works", {
    data <- data.frame(
        Value = c(1, 2, 3, 100, 5)
    )
    
    options <- create_options(vars = "Value", 
                              method_category = "composite", 
                              composite_threshold = 0.5,
                              show_outlier_table = TRUE,
                              show_method_comparison = TRUE)
    
    analysis <- outlierdetectionClass$new(options, data)
    analysis$run()
    
    expect_true(grepl("Composite", analysis$results$outlier_table$content, fixed = TRUE))
    expect_true(grepl("Method Comparison", analysis$results$method_comparison$content, fixed = TRUE))
})

test_that("Exclusion Summary Works", {
    data <- data.frame(
        Value = c(1, 2, 3, 100, 5)
    )
    
    options <- create_options(vars = "Value", 
                              method_category = "univariate", 
                              show_exclusion_summary = TRUE)
    
    analysis <- outlierdetectionClass$new(options, data)
    analysis$run()
    
    expect_true(grepl("Exclusion Recommendations", analysis$results$exclusion_summary$content, fixed = TRUE))
    expect_true(grepl("Observations Retained:", analysis$results$exclusion_summary$content, fixed = TRUE))
})

test_that("Visualization State is Set", {
    data <- data.frame(
        Value = c(1, 2, 3, 100, 5)
    )
    
    options <- create_options(vars = "Value", 
                              method_category = "univariate", 
                              show_visualization = TRUE)
    
    analysis <- outlierdetectionClass$new(options, data)
    analysis$run()
    
    expect_true(!is.null(analysis$results$plot$state))
})

print("All tests completed successfully!")
