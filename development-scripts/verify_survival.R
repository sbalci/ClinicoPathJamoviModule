library(jmvcore)
library(survival)
library(survminer)
library(finalfit)
library(ggplot2)

# Source the function files
source("R/survival.h.R")
source("R/survival.b.R")

# Helper function to run analysis
run_analysis <- function(data, options) {
    analysis <- survivalClass$new(
        options = options,
        data = data
    )
    analysis$run()
    return(analysis)
}

# Create synthetic data
set.seed(123)
n <- 200
# Groups: A, B
group <- sample(c("A", "B"), n, replace = TRUE)
group <- factor(group)

# Survival time and status
# Make group B have better survival
risk <- ifelse(group == "B", 0.5, 1.0)
time <- rexp(n, rate = 0.1 * risk)
status <- sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7))

# Competing risks: 0=censored, 1=event 1, 2=event 2
status_comp <- sample(c(0, 1, 2), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))

data <- data.frame(
    time = time,
    status = status,
    group = group,
    status_comp = status_comp
)

print("Data summary:")
print(head(data))

# Test 1: Basic Survival Analysis
print("\n--- Test 1: Basic Survival Analysis ---")
options_basic <- survivalOptions$new(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    explanatory = "group",
    sc = TRUE,
    ce = TRUE,
    ch = TRUE,
    loglog = TRUE,
    pw = TRUE,
    ph_cox = TRUE,
    residual_diagnostics = TRUE
)

tryCatch({
    analysis_basic <- run_analysis(data, options_basic)
    print("Basic analysis ran successfully.")
    print("Median Table:")
    print(analysis_basic$results$medianTable$asDF)
    print("Cox Table:")
    print(analysis_basic$results$coxTable$asDF)
}, error = function(e) {
    print(paste("Error in basic analysis:", e$message))
})

# Test 2: Person-Time Analysis
print("\n--- Test 2: Person-Time Analysis ---")
options_pt <- survivalOptions$new(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    explanatory = "group",
    person_time = TRUE,
    rate_multiplier = 100,
    time_intervals = "10, 20, 30"
)

tryCatch({
    analysis_pt <- run_analysis(data, options_pt)
    print("Person-Time analysis ran successfully.")
    print("Person-Time Table:")
    print(analysis_pt$results$personTimeTable$asDF)
}, error = function(e) {
    print(paste("Error in person-time analysis:", e$message))
})

# Test 3: RMST Analysis
print("\n--- Test 3: RMST Analysis ---")
options_rmst <- survivalOptions$new(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = 1,
    explanatory = "group",
    rmst_analysis = TRUE,
    rmst_tau = 20
)

tryCatch({
    analysis_rmst <- run_analysis(data, options_rmst)
    print("RMST analysis ran successfully.")
    print("RMST Table:")
    print(analysis_rmst$results$rmstTable$asDF)
}, error = function(e) {
    print(paste("Error in RMST analysis:", e$message))
})

# Test 4: Competing Risks Analysis
print("\n--- Test 4: Competing Risks Analysis ---")
options_comp <- survivalOptions$new(
    elapsedtime = "time",
    outcome = "status_comp",
    multievent = TRUE,
    analysistype = "compete",
    dod = 1,
    dooc = 2,
    awd = 0, # Assuming 0 is censored
    explanatory = "group"
)

tryCatch({
    analysis_comp <- run_analysis(data, options_comp)
    print("Competing risks analysis ran successfully.")
    print("Median Table (Competing Risk):")
    print(analysis_comp$results$medianTable$asDF)
}, error = function(e) {
    print(paste("Error in competing risks analysis:", e$message))
})

print("\nVerification complete!")