# Isolated Multivariate Test for outlierdetection

library(R6)
library(ggplot2)

# Mock Classes
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

source("R/outlierdetection.b.R")

# Test
data <- data.frame(
    X = c(1, 2, 3, 100, 5, 6, 7, 8, 9, 10),
    Y = c(1, 3, 2, 100, 4, 7, 5, 9, 8, 11)
)

options <- list(
    vars = c("X", "Y"),
    method_category = "multivariate",
    univariate_methods = "zscore_robust",
    multivariate_methods = "mahalanobis_robust",
    composite_threshold = 0.5,
    zscore_threshold = 3.29,
    iqr_multiplier = 1.7,
    confidence_level = 0.999,
    show_outlier_table = TRUE,
    show_method_comparison = FALSE,
    show_exclusion_summary = FALSE,
    show_visualization = FALSE,
    show_interpretation = FALSE
)

print("Running multivariate test...")
analysis <- outlierdetectionClass$new(options, data)
analysis$run()

print("Content:")
print(analysis$results$outlier_table$content)

print("Checking for 'Robust Mahalanobis'...")
if (grepl("Robust Mahalanobis", analysis$results$outlier_table$content)) {
    print("✓ PASS: Found 'Robust Mahalanobis'")
} else {
    print("✗ FAIL: Did not find 'Robust Mahalanobis'")
}

print("Checking for 'Outliers Detected: 1'...")
if (grepl("Outliers Detected: 1", analysis$results$outlier_table$content)) {
    print("✓ PASS: Found 'Outliers Detected: 1'")
} else {
    print("✗ FAIL: Did not find 'Outliers Detected: 1'")
}

print("Test complete!")
