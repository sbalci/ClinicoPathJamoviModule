
# Verification script for recist
library(jmvcore)
library(R6)
library(dplyr)
library(ggplot2)

# Source the function files
source("R/recist.h.R")
source("R/recist.b.R")

# Helper function to run analysis
run_analysis <- function(data, options) {
    analysis <- recistClass$new(
        options = options,
        data = data
    )
    analysis$run()
    return(analysis)
}

# Scenario 1: Basic PR Case
print("\n--- Test 1: Basic PR Case ---")
data_pr <- data.frame(
    patient = rep("P1", 6),
    time = rep(c(0, 6, 12), each = 2),
    lesion = rep(c("L1", "L2"), 3),
    type = "target",
    diameter = c(
        20, 30, # Baseline: Sum = 50
        15, 20, # Time 6: Sum = 35 (-30% -> PR)
        10, 15  # Time 12: Sum = 25 (-50% -> PR)
    ),
    organ = "Lung",
    stringsAsFactors = FALSE
)

options_pr <- recistOptions$new(
    patientId = "patient",
    assessmentTime = "time",
    lesionId = "lesion",
    lesionType = "type",
    lesionDiameter = "diameter",
    organ = "organ"
)

tryCatch({
    analysis_pr <- run_analysis(data_pr, options_pr)
    print("PR analysis ran successfully.")
    print("Response Table:")
    print(analysis_pr$results$responseTable$asDF)
    print("Best Response Table:")
    print(analysis_pr$results$bestResponseTable$asDF)
}, error = function(e) {
    print(paste("Error in PR analysis:", e$message))
})

# Scenario 2: PD Case (Nadir Reference)
print("\n--- Test 2: PD Case (Nadir) ---")
data_pd <- data.frame(
    patient = rep("P2", 6),
    time = rep(c(0, 6, 12), each = 2),
    lesion = rep(c("L1", "L2"), 3),
    type = "target",
    diameter = c(
        20, 30, # Baseline: Sum = 50
        10, 15, # Time 6: Sum = 25 (Nadir)
        15, 20  # Time 12: Sum = 35 (+40% from Nadir, +10mm -> PD)
    ),
    organ = "Lung",
    stringsAsFactors = FALSE
)

options_pd <- recistOptions$new(
    patientId = "patient",
    assessmentTime = "time",
    lesionId = "lesion",
    lesionType = "type",
    lesionDiameter = "diameter",
    organ = "organ",
    nadirReference = TRUE
)

tryCatch({
    analysis_pd <- run_analysis(data_pd, options_pd)
    print("PD analysis ran successfully.")
    print("Response Table:")
    print(analysis_pd$results$responseTable$asDF)
}, error = function(e) {
    print(paste("Error in PD analysis:", e$message))
})

# Scenario 3: New Lesion -> PD
print("\n--- Test 3: New Lesion PD ---")
data_new <- data.frame(
    patient = rep("P3", 4),
    time = c(0, 0, 6, 6),
    lesion = c("L1", "L2", "L1", "New1"),
    type = c("target", "target", "target", "new"),
    diameter = c(20, 30, 20, NA),
    organ = "Lung",
    stringsAsFactors = FALSE
)

options_new <- recistOptions$new(
    patientId = "patient",
    assessmentTime = "time",
    lesionId = "lesion",
    lesionType = "type",
    lesionDiameter = "diameter",
    organ = "organ"
)

tryCatch({
    analysis_new <- run_analysis(data_new, options_new)
    print("New Lesion analysis ran successfully.")
    print("Response Table:")
    print(analysis_new$results$responseTable$asDF)
}, error = function(e) {
    print(paste("Error in New Lesion analysis:", e$message))
})

# Scenario 4: Non-Target PD -> Overall PD
print("\n--- Test 4: Non-Target PD ---")
data_nt <- data.frame(
    patient = rep("P4", 4),
    time = c(0, 0, 6, 6),
    lesion = c("L1", "NT1", "L1", "NT1"),
    type = c("target", "non-target", "target", "non-target"),
    diameter = c(20, NA, 15, NA),
    status = c(NA, "present", NA, "unequivocal progression"),
    organ = "Lung",
    stringsAsFactors = FALSE
)

options_nt <- recistOptions$new(
    patientId = "patient",
    assessmentTime = "time",
    lesionId = "lesion",
    lesionType = "type",
    lesionDiameter = "diameter",
    nonTargetStatus = "status",
    organ = "organ"
)

tryCatch({
    analysis_nt <- run_analysis(data_nt, options_nt)
    print("Non-Target PD analysis ran successfully.")
    print("Response Table:")
    print(analysis_nt$results$responseTable$asDF)
}, error = function(e) {
    print(paste("Error in Non-Target PD analysis:", e$message))
})

# Scenario 5: Max Lesions Rule
print("\n--- Test 5: Max Lesions Rule ---")
# 6 lesions in one organ (should pick top 2)
data_max <- data.frame(
    patient = rep("P5", 6),
    time = rep(0, 6),
    lesion = paste0("L", 1:6),
    type = "target",
    diameter = c(10, 20, 30, 40, 50, 60), # Should pick 60 and 50
    organ = rep("Liver", 6),
    stringsAsFactors = FALSE
)

options_max <- recistOptions$new(
    patientId = "patient",
    assessmentTime = "time",
    lesionId = "lesion",
    lesionType = "type",
    lesionDiameter = "diameter",
    organ = "organ",
    maxPerOrgan = 2
)

tryCatch({
    analysis_max <- run_analysis(data_max, options_max)
    print("Max Lesions analysis ran successfully.")
    print("Target Sum Table:")
    print(analysis_max$results$targetSumTable$asDF)
    # Expected sum: 60 + 50 = 110
}, error = function(e) {
    print(paste("Error in Max Lesions analysis:", e$message))
})

# Scenario 6: Missing Target Lesion (NE check)
print("\n--- Test 6: Missing Target Lesion ---")
data_ne <- data.frame(
    patient = rep("P6", 4),
    time = rep(c(0, 6), each = 2),
    lesion = rep(c("L1", "L2"), 2),
    type = "target",
    diameter = c(20, 20, 20, NA), # Time 6: L2 is missing
    organ = "Lung",
    stringsAsFactors = FALSE
)

options_ne <- recistOptions$new(
    patientId = "patient",
    assessmentTime = "time",
    lesionId = "lesion",
    lesionType = "type",
    lesionDiameter = "diameter",
    organ = "organ"
)

tryCatch({
    analysis_ne <- run_analysis(data_ne, options_ne)
    print("Missing Lesion analysis ran successfully.")
    print("Target Sum Table:")
    print(analysis_ne$results$targetSumTable$asDF)
    print("Response Table:")
    print(analysis_ne$results$responseTable$asDF)
}, error = function(e) {
    print(paste("Error in Missing Lesion analysis:", e$message))
})

# Scenario 7: NE Only
print("\n--- Test 7: NE Only ---")
data_ne_only <- data.frame(
    patient = rep("P7", 2),
    time = c(0, 6),
    lesion = "L1",
    type = "target",
    diameter = c(20, NA),
    organ = "Lung",
    stringsAsFactors = FALSE
)

options_ne_only <- recistOptions$new(
    patientId = "patient",
    assessmentTime = "time",
    lesionId = "lesion",
    lesionType = "type",
    lesionDiameter = "diameter",
    organ = "organ"
)

tryCatch({
    analysis_ne_only <- run_analysis(data_ne_only, options_ne_only)
    print("NE Only analysis ran successfully.")
    print("Best Response Table:")
    print(analysis_ne_only$results$bestResponseTable$asDF)
}, error = function(e) {
    print(paste("Error in NE Only analysis:", e$message))
})

print("\nVerification complete!")
