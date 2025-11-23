
# Verification Script for stagemigration function

# Load necessary libraries
if (!requireNamespace("jmvcore", quietly = TRUE)) stop("jmvcore not installed")
if (!requireNamespace("survival", quietly = TRUE)) stop("survival not installed")
if (!requireNamespace("survminer", quietly = TRUE)) stop("survminer not installed")
if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr not installed")

# Source the R file
source("R/stagemigration.h.R")
source("R/stagemigration.b.R")

# Helper function to run analysis
run_stagemigration <- function(data, options) {
    # Use the generated wrapper function
    # We need to pass options as individual arguments
    do.call(stagemigration, c(list(data = data), options))
}

# --- Test Case 1: Will Rogers Phenomenon ---
cat("\n--- Test Case 1: Will Rogers Phenomenon ---\n")

# Create synthetic data
set.seed(123)
n <- 200

# Base survival for stages (Stage I > Stage II > Stage III)
# We will create a "migrating" group from Stage I to Stage II
# Migrators will have survival between Stage I and Stage II

# Group 1: True Stage I (High survival)
n1 <- 80
time1 <- rexp(n1, rate = 0.02) # Mean 50 months
event1 <- sample(c(0, 1), n1, replace = TRUE, prob = c(0.2, 0.8)) # 20% censoring
old1 <- rep("I", n1)
new1 <- rep("I", n1)

# Group 2: Migrators (Old I -> New II) (Medium survival)
# Worse than True I, Better than True II
n2 <- 40
time2 <- rexp(n2, rate = 0.04) # Mean 25 months
event2 <- sample(c(0, 1), n2, replace = TRUE, prob = c(0.2, 0.8)) # 20% censoring
old2 <- rep("I", n2)
new2 <- rep("II", n2)

# Group 3: True Stage II (Low survival)
n3 <- 80
time3 <- rexp(n3, rate = 0.08) # Mean 12.5 months
event3 <- sample(c(0, 1), n3, replace = TRUE, prob = c(0.2, 0.8)) # 20% censoring
old3 <- rep("II", n3)
new3 <- rep("II", n3)

data_wr <- data.frame(
    time = c(time1, time2, time3),
    event = c(event1, event2, event3),
    old_stage = factor(c(old1, old2, old3), levels = c("I", "II")),
    new_stage = factor(c(new1, new2, new3), levels = c("I", "II"))
)

# Options for Will Rogers analysis
options_wr <- list(
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "event",
    eventLevel = "1",
    analysisType = "comprehensive",
    advancedMigrationAnalysis = TRUE,
    showWillRogersAnalysis = TRUE,
    calculateSME = TRUE,
    showExplanations = FALSE
)

# Run analysis
results_wr <- run_stagemigration(data_wr, options_wr)

# Check Will Rogers results
wr_table <- results_wr$willRogersAnalysis
if (!is.null(wr_table)) {
    cat("Will Rogers Analysis Table:\n")
    print(wr_table$asDF)
    
    # Check if migration I -> II is detected as Will Rogers
    # Row key might be "I_to_II"
    rows <- wr_table$asDF
    mig_row <- rows[rows$Migration_Pattern == "I -> II", ]
    
    if (nrow(mig_row) > 0) {
        cat("Migration I -> II Evidence:", mig_row$Will_Rogers_Evidence, "\n")
        if (grepl("Strong", mig_row$Will_Rogers_Evidence) || grepl("Possible", mig_row$Will_Rogers_Evidence)) {
            cat("SUCCESS: Will Rogers phenomenon detected.\n")
        } else {
            cat("FAILURE: Will Rogers phenomenon NOT detected.\n")
        }
    } else {
        cat("FAILURE: Migration row not found.\n")
    }
} else {
    cat("FAILURE: Will Rogers table not generated.\n")
}

# Check SME results
sme_table <- results_wr$stageMigrationEffect
if (!is.null(sme_table)) {
    cat("\nStage Migration Effect Table:\n")
    print(sme_table$asDF)
} else {
    cat("FAILURE: SME table not generated.\n")
}


# --- Test Case 2: Basic Functionality & Options ---
cat("\n--- Test Case 2: Basic Functionality ---\n")

# Simple random data
data_basic <- data.frame(
    time = rexp(100),
    event = sample(0:1, 100, replace = TRUE),
    old = factor(sample(c("A", "B"), 100, replace = TRUE)),
    new = factor(sample(c("A", "B"), 100, replace = TRUE))
)

options_basic <- list(
    oldStage = "old",
    newStage = "new",
    survivalTime = "time",
    event = "event",
    eventLevel = "1",
    analysisType = "basic",
    showMigrationOverview = TRUE,
    showMigrationMatrix = TRUE
)

results_basic <- run_stagemigration(data_basic, options_basic)

if (!is.null(results_basic$migrationOverview)) {
    cat("SUCCESS: Basic analysis ran.\n")
} else {
    cat("FAILURE: Basic analysis failed.\n")
}

