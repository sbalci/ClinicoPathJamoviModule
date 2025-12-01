
# Verification script for irecist
library(jmvcore)
library(R6)
library(dplyr)
library(ggplot2)
library(tidyr)

# Source the function files
source("R/irecist.h.R")
source("R/irecist.b.R")

# Helper function to run analysis
run_analysis <- function(data, options) {
    analysis <- irecistClass$new(
        options = options,
        data = data
    )
    analysis$run()
    return(analysis)
}

# Scenario 1: Responder (iPR -> iCR)
# Baseline: 100mm
# Week 8: 60mm (-40%, iPR)
# Week 16: 0mm (-100%, iCR)
data_responder <- data.frame(
    patient = c("P1", "P1", "P1"),
    time = c(0, 8, 16),
    target_sum = c(100, 60, 0),
    new_lesions = c(0, 0, 0),
    stringsAsFactors = FALSE
)

# Scenario 2: Confirmed Progression (iUPD -> iCPD)
# Baseline: 100mm
# Week 8: 130mm (+30%, iUPD)
# Week 12: 140mm (Confirmed, iCPD)
data_cpd <- data.frame(
    patient = c("P2", "P2", "P2"),
    time = c(0, 8, 12),
    target_sum = c(100, 130, 140),
    new_lesions = c(0, 0, 0),
    stringsAsFactors = FALSE
)

# Scenario 3: Pseudoprogression (iUPD -> iSD)
# Baseline: 100mm
# Week 8: 130mm (+30%, iUPD)
# Week 12: 110mm (+10% from baseline, iSD)
data_pseudo <- data.frame(
    patient = c("P3", "P3", "P3"),
    time = c(0, 8, 12),
    target_sum = c(100, 130, 110),
    new_lesions = c(0, 0, 0),
    stringsAsFactors = FALSE
)

# Scenario 4: New Lesion (iUPD)
# Baseline: 100mm
# Week 8: 100mm (Stable sum) but New Lesion = 1 -> iUPD
data_newlesion <- data.frame(
    patient = c("P4", "P4"),
    time = c(0, 8),
    target_sum = c(100, 100),
    new_lesions = c(0, 1),
    stringsAsFactors = FALSE
)

# Combine data
data <- rbind(data_responder, data_cpd, data_pseudo, data_newlesion)

# Run Analysis
print("\n--- Running iRECIST Analysis ---")
options <- irecistOptions$new(
    patientId = "patient",
    assessmentTime = "time",
    targetLesionSum = "target_sum",
    newLesions = "new_lesions",
    confirmationWindow = 4,
    confirmationWindowMax = 12,
    prThreshold = 30,
    pdThreshold = 20,
    pdAbsolute = 5,
    trackPseudoprogression = TRUE,
    requireConfirmation = TRUE,
    showResponseTable = TRUE,
    showBestResponse = TRUE
)

tryCatch({
    analysis <- run_analysis(data, options)
    print("Analysis ran successfully.")
    
    print("\n--- Response Table ---")
    print(analysis$results$responseTable$asDF)
    
    print("\n--- Best Response Table ---")
    print(analysis$results$bestResponseTable$asDF)
    
    print("\n--- Pseudoprogression Table ---")
    print(analysis$results$pseudoprogressionTable$asDF)
    
}, error = function(e) {
    print(paste("Error in analysis:", e$message))
})

print("\nVerification complete!")
