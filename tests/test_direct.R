# Simple Direct Test for outlierdetection

library(R6)
library(ggplot2)

# Minimal mocks
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

# Run all 5 tests
cat("\\n=== Running 5 Direct Tests ===\\n\\n")

# Test 1: Univariate
cat("Test 1: Univariate Outlier Detection...\\n")
data1 <- data.frame(Value = c(1, 2, 3, 100, 5))
options1 <- list(vars = "Value", method_category = "univariate", univariate_methods = "zscore_robust",
                multivariate_methods = "mahalanobis", composite_threshold = 0.5, zscore_threshold = 3.29,
                iqr_multiplier = 1.7, confidence_level = 0.999, show_outlier_table = TRUE,
                show_method_comparison = FALSE, show_exclusion_summary = FALSE, show_visualization = FALSE,
                show_interpretation = FALSE)
analysis1 <- outlierdetectionClass$new(options1, data1)
analysis1$run()
if (grepl("Outliers Detected: 1", analysis1$results$outlier_table$content, fixed = TRUE)) {
    cat("✓ PASS\\n\\n")
} else {
    cat("✗ FAIL\\n\\n")
}

# Test 2: Multivariate
cat("Test 2: Multivariate Outlier Detection...\\n")
data2 <- data.frame(X = c(1, 2, 3, 100, 5, 6, 7, 8, 9, 10), Y = c(1, 3, 2, 100, 4, 7, 5, 9, 8, 11))
options2 <- list(vars = c("X", "Y"), method_category = "multivariate", univariate_methods = "zscore_robust",
                multivariate_methods = "mahalanobis_robust", composite_threshold = 0.5, zscore_threshold = 3.29,
                iqr_multiplier = 1.7, confidence_level = 0.999, show_outlier_table = TRUE,
                show_method_comparison = FALSE, show_exclusion_summary = FALSE, show_visualization = FALSE,
                show_interpretation = FALSE)
analysis2 <- outlierdetectionClass$new(options2, data2)
analysis2$run()
if (grepl("Robust Mahalanobis", analysis2$results$outlier_table$content, fixed = TRUE) &&
    grepl("Outliers Detected: 1", analysis2$results$outlier_table$content, fixed = TRUE)) {
    cat("✓ PASS\\n\\n")
} else {
    cat("✗ FAIL\\n\\n")
}

# Test 3: Composite
cat("Test 3: Composite Outlier Detection...\\n")
data3 <- data.frame(Value = c(1, 2, 3, 100, 5))
options3 <- list(vars = "Value", method_category = "composite", univariate_methods = "zscore_robust",
                multivariate_methods = "mahalanobis", composite_threshold = 0.5, zscore_threshold = 3.29,
                iqr_multiplier = 1.7, confidence_level = 0.999, show_outlier_table = TRUE,
                show_method_comparison = TRUE, show_exclusion_summary = FALSE, show_visualization = FALSE,
                show_interpretation = FALSE)
analysis3 <- outlierdetectionClass$new(options3, data3)
analysis3$run()
if (grepl("Composite", analysis3$results$outlier_table$content, fixed = TRUE) &&
    grepl("Method Comparison", analysis3$results$method_comparison$content, fixed = TRUE)) {
    cat("✓ PASS\\n\\n")
} else {
    cat("✗ FAIL\\n\\n")
}

# Test 4: Exclusion Summary
cat("Test 4: Exclusion Summary...\\n")
data4 <- data.frame(Value = c(1, 2, 3, 100, 5))
options4 <- list(vars = "Value", method_category = "univariate", univariate_methods = "zscore_robust",
                multivariate_methods = "mahalanobis", composite_threshold = 0.5, zscore_threshold = 3.29,
                iqr_multiplier = 1.7, confidence_level = 0.999, show_outlier_table = FALSE,
                show_method_comparison = FALSE, show_exclusion_summary = TRUE, show_visualization = FALSE,
                show_interpretation = FALSE)
analysis4 <- outlierdetectionClass$new(options4, data4)
analysis4$run()
if (grepl("Exclusion Recommendations", analysis4$results$exclusion_summary$content, fixed = TRUE) &&
    grepl("Observations Retained: 4", analysis4$results$exclusion_summary$content, fixed = TRUE)) {
    cat("✓ PASS\\n\\n")
} else {
    cat("✗ FAIL\\n\\n")
}

# Test 5: Visualization
cat("Test 5: Visualization State...\\n")
data5 <- data.frame(Value = c(1, 2, 3, 100, 5))
options5 <- list(vars = "Value", method_category = "univariate", univariate_methods = "zscore_robust",
                multivariate_methods = "mahalanobis", composite_threshold = 0.5, zscore_threshold = 3.29,
                iqr_multiplier = 1.7, confidence_level = 0.999, show_outlier_table = FALSE,
                show_method_comparison = FALSE, show_exclusion_summary = FALSE, show_visualization = TRUE,
                show_interpretation = FALSE)
analysis5 <- outlierdetectionClass$new(options5, data5)
analysis5$run()
if (!is.null(analysis5$results$plot$state)) {
    cat("✓ PASS\\n\\n")
} else {
    cat("✗ FAIL\\n\\n")
}

cat("=== All Direct Tests Complete ===\\n")
