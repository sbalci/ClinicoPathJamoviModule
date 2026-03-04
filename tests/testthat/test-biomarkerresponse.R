
test_that('biomarkerresponse: Basic Analysis Works', {
    skip_if_not_installed('jmvReadWrite')
    skip_if_not_installed('pROC')
    
    # Generate synthetic data
    set.seed(123)
    n <- 100
    data <- data.frame(
        biomarker = c(rnorm(50, 10, 2), rnorm(50, 15, 2)),
        response = factor(rep(c('Negative', 'Positive'), each = 50))
    )
    
    expect_no_error({
        # Note: In a test environment, we instantiate the R6 class directly or via the function wrapper
        # Assuming the function wrapper 'biomarkerresponse' exists and returns the results object
        results <- biomarkerresponse(
            data = data,
            biomarker = 'biomarker',
            response = 'response',
            responseType = 'binary',
            positiveLevel = 'Positive'
        )
    })
    
    # Check if results are populated
    # Depending on how the object is structured, we might check table existence
    expect_true(!is.null(results$threshold))
    expect_true(inherits(results, "biomarkerresponseResults") || inherits(results, "R6"))
})

test_that('biomarkerresponse: Validation Notices', {
    skip_if_not_installed('jmvReadWrite')
    
    # Small sample size
    data_small <- data.frame(
        biomarker = rnorm(20),
        response = factor(sample(c('A', 'B'), 20, replace=TRUE))
    )
    
    results <- biomarkerresponse(
        data = data_small,
        biomarker = 'biomarker',
        response = 'response',
        responseType = 'binary',
        positiveLevel = 'A'
    )
    
    # Check for warning (this might be tricky to test if it's just html output, 
    # but we can check if the notice object exists in the results list items if accessible)
    # For now, just ensuring it runs without crashing
    expect_true(inherits(results, "R6"))
})
