
# Test Bayesian convergence logic
library(R6)
nogoldstandardBase <- R6::R6Class("nogoldstandardBase")
source("R/nogoldstandard.b.R")

# Mock options
options <- list(
    method = "bayesian",
    bootstrap = FALSE,
    nboot = 100,
    alpha = 0.05,
    verbose = FALSE,
    test1 = "T1",
    test1Positive = "1",
    test2 = "T2",
    test2Positive = "1"
)

# Create binary data with NAs
set.seed(123)
n <- 50
binary_data <- data.frame(
    T1 = rbinom(n, 1, 0.7),
    T2 = rbinom(n, 1, 0.6)
)
# Introduce NAs
binary_data[1:5, 1] <- NA
binary_data[6:10, 2] <- NA

# Mock the class
mock_class <- R6::R6Class(
    "mock_class",
    inherit = nogoldstandardClass,
    public = list(
        initialize = function() {},
        test_bayesian = function(data) {
            private$.runBayesian(data)
        }
    )
)

# Run the test
instance <- mock_class$new()
# Manually inject options (since we bypassed jmvcore)
instance$options <- options

# Call the private method
tryCatch({
    result <- instance$test_bayesian(binary_data)
    print("Bayesian run successful")
    print(paste("Prevalence:", result$prevalence))
    print(paste("Converged:", result$converged))
    print("Sensitivities:")
    print(result$sensitivities)
}, error = function(e) {
    print(paste("Error:", e$message))
})
