
# Source the necessary R files
source("../../R/outlierdetection.h.R")
source("../../R/outlierdetection.b.R")

test_that("warnings panel captures small sample size", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # Create small dataset (N < 30)
    data <- data.frame(
        x = rnorm(20),
        y = rnorm(20)
    )
    
    # Initialize module
    options <- outlierdetectionOptions$new(
        vars = c("x", "y"),
        method_category = "composite"
    )
    
    # Run analysis
    analysis <- outlierdetectionClass$new(options = options, data = data)
    analysis$run()
    
    # Check if warnings result populated
    # Note: setContent in .b.R populates the warnings result
    # We inspect the private messages or the result object
    
    # In Jamovi unit tests, we can access results via $results
    # The warnings are in $results$warnings
    
    # Access the HTML content
    warnings_html <- analysis$results$warnings$content
    
    # Verify warning message present
    expect_true(!is.null(warnings_html))
    expect_true(grepl("Sample size is small", warnings_html))
})

test_that("skewness warning triggers for standard methods on skewed data", {
    # Create skewed var (log-normalish)
    set.seed(123)
    data <- data.frame(
        skewed = exp(rnorm(100)) 
    )
    
    # Use standard Z-score which should trigger warning
    options <- outlierdetectionOptions$new(
        vars = c("skewed"),
        method_category = "univariate",
        univariate_methods = "zscore"
    )
    
    analysis <- outlierdetectionClass$new(options = options, data = data)
    analysis$run()
    
    warnings_html <- analysis$results$warnings$content
    expect_true(grepl("highly skewed", warnings_html))
    expect_true(grepl("Consider using Robust Z-score", warnings_html))
})

test_that("composite method comparison table generates", {
    data <- data.frame(
        x = c(rnorm(98), 10, 12), # 2 obvious outliers
        y = c(rnorm(98), 10, 12)
    )
    
    options <- outlierdetectionOptions$new(
        vars = c("x", "y"),
        method_category = "composite",
        show_method_comparison = TRUE
    )
    
    analysis <- outlierdetectionClass$new(options = options, data = data)
    analysis$run()
    
    comparison_html <- analysis$results$method_comparison$content
    
    # Check if table headers for breakdown exist
    expect_true(grepl("Agreement Rate", comparison_html))
    expect_true(grepl("Outliers Detected", comparison_html))
    
    # Check if a method is listed (e.g. Mahalanobis or Z_Score_Robust)
    # The exact name depends on performance pkg version but usually contains method names
    # e.g. "Outlier_Zscore", "Distance_Zscore", "Mahalanobis"
    # We check for generic patterns or specific known outputs from performance
    expect_true(any(sapply(c("Zscore", "IQR", "Mahalanobis", "Robust"), function(p) grepl(p, comparison_html, ignore.case = TRUE))))
})
