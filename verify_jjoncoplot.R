
# Verification script for jjoncoplot
library(jmvcore)
library(R6)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# Source the function files
source("R/jjoncoplot.h.R")
source("R/jjoncoplot.b.R")

# Helper function to run analysis
run_analysis <- function(data, options) {
    analysis <- jjoncoplotClass$new(
        options = options,
        data = data
    )
    analysis$run()
    return(analysis)
}

# Generate synthetic data
set.seed(123)
n_samples <- 20
data <- data.frame(
    SampleID = paste0("S", 1:n_samples),
    TP53 = sample(c(0, 1), n_samples, replace = TRUE, prob = c(0.6, 0.4)),
    KRAS = sample(c(0, 1), n_samples, replace = TRUE, prob = c(0.7, 0.3)),
    PIK3CA = sample(c(0, 1), n_samples, replace = TRUE, prob = c(0.8, 0.2)),
    EGFR = sample(c(0, 1), n_samples, replace = TRUE, prob = c(0.9, 0.1)),
    Age = rnorm(n_samples, 60, 10),
    Stage = sample(c("I", "II", "III", "IV"), n_samples, replace = TRUE),
    stringsAsFactors = FALSE
)

# Test 1: Basic Oncoplot
print("\n--- Test 1: Basic Oncoplot ---")
options_basic <- jjoncoplotOptions$new(
    sampleVar = "SampleID",
    geneVars = c("TP53", "KRAS", "PIK3CA", "EGFR"),
    plotType = "oncoplot",
    showLegend = TRUE
)

tryCatch({
    analysis_basic <- run_analysis(data, options_basic)
    print("Basic analysis ran successfully.")
    print("Mutation Summary:")
    print(analysis_basic$results$mutationSummary$asDF)
}, error = function(e) {
    print(paste("Error in Basic analysis:", e$message))
})

# Test 2: Frequency Plot
print("\n--- Test 2: Frequency Plot ---")
options_freq <- jjoncoplotOptions$new(
    sampleVar = "SampleID",
    geneVars = c("TP53", "KRAS", "PIK3CA", "EGFR"),
    plotType = "frequency"
)

tryCatch({
    analysis_freq <- run_analysis(data, options_freq)
    print("Frequency analysis ran successfully.")
}, error = function(e) {
    print(paste("Error in Frequency analysis:", e$message))
})

# Test 3: Co-occurrence Plot
print("\n--- Test 3: Co-occurrence Plot ---")
options_cooc <- jjoncoplotOptions$new(
    sampleVar = "SampleID",
    geneVars = c("TP53", "KRAS", "PIK3CA", "EGFR"),
    plotType = "cooccurrence"
)

tryCatch({
    analysis_cooc <- run_analysis(data, options_cooc)
    print("Co-occurrence analysis ran successfully.")
    print("Co-occurrence Table:")
    print(analysis_cooc$results$cooccurrence$asDF)
}, error = function(e) {
    print(paste("Error in Co-occurrence analysis:", e$message))
})

# Test 4: Clinical Annotations and TMB
print("\n--- Test 4: Clinical Annotations and TMB ---")
options_clinical <- jjoncoplotOptions$new(
    sampleVar = "SampleID",
    geneVars = c("TP53", "KRAS", "PIK3CA", "EGFR"),
    clinicalVars = c("Stage"),
    showTMB = TRUE,
    showMutationLoad = TRUE,
    drawMarginalPlots = TRUE
)

tryCatch({
    analysis_clinical <- run_analysis(data, options_clinical)
    print("Clinical analysis ran successfully.")
    print("Sample Summary:")
    print(analysis_clinical$results$sampleSummary$asDF)
}, error = function(e) {
    print(paste("Error in Clinical analysis:", e$message))
})

print("\nVerification complete!")
