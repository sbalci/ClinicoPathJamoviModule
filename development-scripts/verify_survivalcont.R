library(jmvcore)
library(survival)
library(survminer)
library(finalfit)
library(ggplot2)

# Source the function files
source("R/survivalcont.h.R")
source("R/survivalcont.b.R")

# Helper function to run analysis
run_analysis <- function(data, options) {
    analysis <- survivalcontClass$new(
        options = options,
        data = data
    )
    analysis$run()
    return(analysis)
}

# Create synthetic data
set.seed(123)
n <- 200
age <- rnorm(n, mean = 60, sd = 10)
time <- rexp(n, rate = 0.1 * (age / 60)) # Survival depends on age
status <- sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7))

# Competing risks: 0=censored, 1=event 1, 2=event 2
status_comp <- sample(c(0, 1, 2), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))

data <- data.frame(
    time = time,
    status = status,
    age = age,
    status_comp = status_comp
)

print("Data summary:")
print(head(data))

# Test 1: Basic Analysis with Optimal Cut-off
print("\n--- Test 1: Basic Analysis with Optimal Cut-off ---")
options_basic <- survivalcontOptions$new(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    contexpl = "age",
    findcut = TRUE,
    sc = TRUE,
    ce = TRUE,
    ch = TRUE,
    kmunicate = TRUE,
    loglog = TRUE,
    ci95 = TRUE,
    risktable = TRUE,
    calculatedcutoff = TRUE
)

tryCatch({
    analysis_basic <- run_analysis(data, options_basic)
    print("Basic analysis ran successfully.")
    print("Cox Table:")
    print(analysis_basic$results$coxTable$asDF)
    print("Cut Point Table:")
    print(analysis_basic$results$rescutTable$asDF)
    print("Median Table:")
    print(analysis_basic$results$medianTable$asDF)
}, error = function(e) {
    print(paste("Error in basic analysis:", e$message))
})

# Test 2: Multiple Cut-offs Analysis
print("\n--- Test 2: Multiple Cut-offs Analysis ---")
options_multi <- survivalcontOptions$new(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    contexpl = "age",
    multiple_cutoffs = TRUE,
    num_cutoffs = "three",
    cutoff_method = "quantile",
    calculatedmulticut = TRUE,
    sc = TRUE
)

tryCatch({
    analysis_multi <- run_analysis(data, options_multi)
    print("Multiple cut-offs analysis ran successfully.")
    print("Multiple Cut Table:")
    print(analysis_multi$results$multipleCutTable$asDF)
    print("Multiple Median Table:")
    print(analysis_multi$results$multipleMedianTable$asDF)
}, error = function(e) {
    print(paste("Error in multiple cut-offs analysis:", e$message))
})

# Test 3: Person-Time and RMST
print("\n--- Test 3: Person-Time and RMST ---")
options_pt <- survivalcontOptions$new(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    contexpl = "age",
    person_time = TRUE,
    rmst_analysis = TRUE,
    rmst_tau = 20,
    residual_diagnostics = TRUE
)

tryCatch({
    analysis_pt <- run_analysis(data, options_pt)
    print("Person-time and RMST analysis ran successfully.")
    print("Person-Time Table:")
    print(analysis_pt$results$personTimeTable$asDF)
    print("RMST Table:")
    print(analysis_pt$results$rmstTable$asDF)
}, error = function(e) {
    print(paste("Error in Person-time/RMST analysis:", e$message))
})

print("\nVerification complete!")