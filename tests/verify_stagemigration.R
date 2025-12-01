
# Verification script for stagemigration
library(jmvcore)
library(R6)
library(survival)

# Source the function files
source("R/stagemigration.h.R")
source("R/stagemigration_helpers.R")
source("R/stagemigration.b.R")

# Helper function to run analysis
run_analysis <- function(data, options) {
    analysis <- stagemigrationClass$new(
        options = options,
        data = data
    )
    analysis$run()
    return(analysis)
}

# Create synthetic data
set.seed(123)
n <- 200
# Old staging: I, II, III
old_stage <- sample(c("I", "II", "III"), n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
old_stage <- factor(old_stage, levels = c("I", "II", "III"))

# New staging: IA, IB, IIA, IIB, III (simulating refinement)
new_stage <- character(n)
for (i in 1:n) {
    if (old_stage[i] == "I") {
        new_stage[i] <- sample(c("IA", "IB"), 1, prob = c(0.7, 0.3))
    } else if (old_stage[i] == "II") {
        new_stage[i] <- sample(c("IB", "IIA", "IIB"), 1, prob = c(0.2, 0.5, 0.3)) # Some downstaging to IB
    } else {
        new_stage[i] <- sample(c("IIB", "III"), 1, prob = c(0.2, 0.8)) # Some downstaging to IIB
    }
}
new_stage <- factor(new_stage, levels = c("IA", "IB", "IIA", "IIB", "III"))

# Survival time and status
# Make survival depend on new stage more than old stage
risk <- as.numeric(new_stage) * 0.5
time <- rexp(n, rate = 0.1 * exp(risk))
status <- sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7))

data <- data.frame(
    old_stage = old_stage,
    new_stage = new_stage,
    time = time,
    status = status
)

print("Data summary:")
print(head(data))
print(table(data$old_stage, data$new_stage))

# Test 1: Basic Analysis
print("\n--- Test 1: Basic Analysis ---")
options_basic <- stagemigrationOptions$new(
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    analysisType = "basic",
    showMigrationMatrix = TRUE,
    showMigrationSummary = TRUE
)

tryCatch({
    analysis_basic <- run_analysis(data, options_basic)
    print("Basic analysis ran successfully.")
    print("Migration Overview:")
    print(analysis_basic$results$migrationOverview$asDF)
    print("Migration Matrix:")
    print(analysis_basic$results$migrationMatrix$asDF)
}, error = function(e) {
    print(paste("Error in basic analysis:", e$message))
})

# Test 2: Standard Analysis with C-index
print("\n--- Test 2: Standard Analysis ---")
options_standard <- stagemigrationOptions$new(
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    analysisType = "standard",
    showStatisticalComparison = TRUE
)

tryCatch({
    analysis_standard <- run_analysis(data, options_standard)
    print("Standard analysis ran successfully.")
    print("Statistical Comparison:")
    print(analysis_standard$results$statisticalComparison$asDF)
}, error = function(e) {
    print(paste("Error in standard analysis:", e$message))
})

# Test 3: Comprehensive Analysis (NRI, IDI)
print("\n--- Test 3: Comprehensive Analysis ---")
# Check if packages are available
if (requireNamespace("survIDINRI", quietly = TRUE)) {
    options_comp <- stagemigrationOptions$new(
        oldStage = "old_stage",
        newStage = "new_stage",
        survivalTime = "time",
        event = "status",
        analysisType = "comprehensive",
        calculateNRI = TRUE,
        calculateIDI = TRUE,
        nriTimePoints = "12, 24"
    )

    tryCatch({
        analysis_comp <- run_analysis(data, options_comp)
        print("Comprehensive analysis ran successfully.")
        if (!is.null(analysis_comp$results$nriResults)) {
            print("NRI Results:")
            print(analysis_comp$results$nriResults$asDF)
        }
    }, error = function(e) {
        print(paste("Error in comprehensive analysis:", e$message))
    })
} else {
    print("Skipping comprehensive analysis (survIDINRI not available)")
}

print("\n--- Test 4: Will Rogers Phenomenon ---")
# Simulate Will Rogers:
# Stage I: Good prognosis (mean survival 50)
# Stage II: Poor prognosis (mean survival 10)
# "Migrators": Intermediate prognosis (mean survival 25) - currently in Stage I, moving to Stage II

set.seed(456)
n_I <- 100
n_II <- 100
n_mig <- 50

# Survival times
time_I <- rexp(n_I, 1/50)
time_II <- rexp(n_II, 1/10)
time_mig <- rexp(n_mig, 1/25)

# Combine
time <- c(time_I, time_II, time_mig)
# Mix of events (1) and censored (0)
status <- sample(c(0, 1), length(time), replace = TRUE, prob = c(0.1, 0.9))

# Old Staging: Migrators are in Stage I
old_stage <- factor(c(rep("I", n_I), rep("II", n_II), rep("I", n_mig)), levels = c("I", "II"))

# New Staging: Migrators are in Stage II
new_stage <- factor(c(rep("I", n_I), rep("II", n_II), rep("II", n_mig)), levels = c("I", "II"))

data_wr <- data.frame(
    old_stage = old_stage,
    new_stage = new_stage,
    time = time,
    status = status
)

options_wr <- stagemigrationOptions$new(
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    analysisType = "standard",
    showWillRogersAnalysis = TRUE,
    advancedMigrationAnalysis = TRUE
)

tryCatch({
    analysis_wr <- run_analysis(data_wr, options_wr)
    print("Will Rogers analysis ran successfully.")
    if (!is.null(analysis_wr$results$willRogersAnalysis)) {
        print("Will Rogers Analysis Table:")
        print(analysis_wr$results$willRogersAnalysis$asDF)
    }
}, error = function(e) {
    print(paste("Error in Will Rogers analysis:", e$message))
})

print("\nVerification complete!")
print(warnings())
