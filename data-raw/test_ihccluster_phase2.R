#!/usr/bin/env Rscript
# Test script for ihccluster Phase 2 features (Sterlacci 2019)
# Tests: Reproducibility testing and Supervised clustering

# Load the module directly from source
devtools::load_all(".")

# Load test data
data_path <- file.path(
    "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data",
    "ihc_breast_cancer.csv"
)

if (!file.exists(data_path)) {
    stop("Test data file not found: ", data_path)
}

data <- read.csv(data_path, stringsAsFactors = TRUE)

cat("=== IHC Clustering Phase 2 Feature Testing ===\n\n")
cat("Data dimensions:", nrow(data), "cases x", ncol(data), "variables\n")
cat("Diagnosis groups (TrueSubtype):", levels(data$TrueSubtype), "\n")
cat("Diagnosis distribution:\n")
print(table(data$TrueSubtype))
cat("\n")

# Define IHC markers
categorical_markers <- c("ER_Status", "PR_Status", "HER2_IHC", "CK5_6", "EGFR")
continuous_markers <- c("Ki67_Percent", "AR_Hscore", "p53_Percent")

cat("Categorical markers:", categorical_markers, "\n")
cat("Continuous markers:", continuous_markers, "\n\n")

# =============================================================================
# TEST 1: Reproducibility Testing (Random Split Validation with Cohen's Kappa)
# =============================================================================

cat("=== TEST 1: REPRODUCIBILITY TESTING ===\n\n")

tryCatch({
    result1 <- ihccluster(
        data = data,
        catVars = categorical_markers,
        contVars = continuous_markers,
        caseId = "PatientID",

        # Clustering options
        method = "pam",
        distanceMethod = "gower",
        nClusters = 3,
        autoSelectK = FALSE,
        scaleContVars = TRUE,
        seed = 42,

        # Phase 2: Reproducibility testing
        reproducibilityTest = TRUE,
        nSplits = 10,

        # Basic outputs
        showSilhouette = TRUE,
        showHeatmap = FALSE,
        clusterQualityMetrics = TRUE,
        associationTests = TRUE,
        multipleTestingCorrection = "bonferroni"
    )

    cat("Test 1 completed successfully!\n")
    cat("Check reproducibilityStats table for Cohen's kappa values.\n\n")

}, error = function(e) {
    cat("ERROR in Test 1:\n")
    cat(as.character(e), "\n\n")
})

# =============================================================================
# TEST 2: Supervised Clustering (Within-Diagnosis Clustering)
# =============================================================================

cat("=== TEST 2: SUPERVISED CLUSTERING ===\n\n")

tryCatch({
    result2 <- ihccluster(
        data = data,
        catVars = categorical_markers,
        contVars = continuous_markers,
        caseId = "PatientID",

        # Clustering options
        method = "hierarchical",
        distanceMethod = "gower",
        linkageMethod = "ward",
        nClusters = 2,
        autoSelectK = FALSE,
        scaleContVars = TRUE,
        seed = 42,

        # Phase 2: Supervised clustering
        supervisedClustering = TRUE,
        supervisedVariable = "TrueSubtype",

        # Basic outputs
        showSilhouette = TRUE,
        showDendrogram = TRUE,
        clusterQualityMetrics = TRUE
    )

    cat("Test 2 completed successfully!\n")
    cat("Check supervisedSummary and supervisedResults for within-group clustering.\n\n")

}, error = function(e) {
    cat("ERROR in Test 2:\n")
    cat(as.character(e), "\n\n")
})

# =============================================================================
# TEST 3: Jaccard Distance with Reproducibility Testing
# =============================================================================

cat("=== TEST 3: JACCARD DISTANCE + REPRODUCIBILITY ===\n\n")

tryCatch({
    result3 <- ihccluster(
        data = data,
        catVars = categorical_markers,
        contVars = continuous_markers,
        caseId = "PatientID",

        # Phase 1: Jaccard distance (binary conversion)
        method = "hierarchical",
        distanceMethod = "jaccard",
        linkageMethod = "complete",
        nClusters = 3,
        autoSelectK = FALSE,
        scaleContVars = TRUE,
        seed = 42,

        # Phase 2: Reproducibility testing
        reproducibilityTest = TRUE,
        nSplits = 10,

        # Outputs
        showSilhouette = TRUE,
        showDendrogram = TRUE,
        associationTests = TRUE,
        multipleTestingCorrection = "bonferroni"
    )

    cat("Test 3 completed successfully!\n")
    cat("Check binaryConversionNotice and reproducibilityStats.\n\n")

}, error = function(e) {
    cat("ERROR in Test 3:\n")
    cat(as.character(e), "\n\n")
})

# =============================================================================
# TEST 4: Combined Phase 2 Features (Reproducibility + Supervised)
# =============================================================================

cat("=== TEST 4: COMBINED PHASE 2 FEATURES ===\n\n")

tryCatch({
    result4 <- ihccluster(
        data = data,
        catVars = categorical_markers,
        contVars = continuous_markers,
        caseId = "PatientID",

        # Clustering options
        method = "pam",
        distanceMethod = "gower",
        nClusters = 3,
        autoSelectK = FALSE,
        scaleContVars = TRUE,
        seed = 42,

        # Phase 2: Both features enabled
        reproducibilityTest = TRUE,
        nSplits = 5,  # Reduced for speed
        supervisedClustering = TRUE,
        supervisedVariable = "TrueSubtype",

        # Outputs
        showSilhouette = TRUE,
        clusterQualityMetrics = TRUE,
        associationTests = TRUE,
        multipleTestingCorrection = "bonferroni"
    )

    cat("Test 4 completed successfully!\n")
    cat("Both reproducibility and supervised clustering should be populated.\n\n")

}, error = function(e) {
    cat("ERROR in Test 4:\n")
    cat(as.character(e), "\n\n")
})

# =============================================================================
# TEST 5: Edge Case - Small Sample for Supervised Clustering
# =============================================================================

cat("=== TEST 5: EDGE CASE - SMALL SAMPLE ===\n\n")

# Subset to only first 20 cases for testing small sample handling
small_data <- data[1:20, ]

tryCatch({
    result5 <- ihccluster(
        data = small_data,
        catVars = categorical_markers[1:3],  # Fewer markers
        contVars = continuous_markers[1],
        caseId = "PatientID",

        # Clustering options
        method = "pam",
        distanceMethod = "gower",
        nClusters = 2,
        autoSelectK = FALSE,
        seed = 42,

        # Phase 2: Should handle small samples gracefully
        reproducibilityTest = TRUE,
        nSplits = 3,
        supervisedClustering = TRUE,
        supervisedVariable = "TrueSubtype"
    )

    cat("Test 5 completed successfully!\n")
    cat("Small sample handling verified.\n\n")

}, error = function(e) {
    cat("ERROR in Test 5:\n")
    cat(as.character(e), "\n\n")
})

cat("=== ALL PHASE 2 TESTS COMPLETED ===\n\n")
cat("Summary:\n")
cat("- Test 1: Reproducibility testing with PAM/Gower\n")
cat("- Test 2: Supervised clustering with hierarchical/Gower\n")
cat("- Test 3: Jaccard distance + reproducibility\n")
cat("- Test 4: Combined Phase 2 features\n")
cat("- Test 5: Small sample edge case\n\n")

cat("If all tests completed successfully, Phase 2 implementation is working correctly.\n")
cat("Manually inspect the following outputs:\n")
cat("  1. reproducibilityStats table (Cohen's kappa values)\n")
cat("  2. supervisedSummary table (clustering by diagnosis)\n")
cat("  3. supervisedResults HTML (detailed clustering info)\n")
cat("  4. binaryConversionNote (for Jaccard tests)\n")
