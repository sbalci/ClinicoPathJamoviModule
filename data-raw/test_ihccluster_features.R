# =============================================================================
# IHC Clustering Feature Testing Script
# =============================================================================
#
# Purpose: Comprehensive testing of all ihccluster features using realistic data
#
# Test Coverage:
# 1. Basic clustering with different methods
# 2. Automatic k-selection vs fixed k
# 3. Mixed categorical and continuous markers
# 4. Missing data handling strategies
# 5. Consensus clustering
# 6. Clinical correlations
# 7. Survival analysis
# 8. Accessibility features (color palettes, fonts)
# 9. Tumor-specific presets
# 10. Internationalization (English/Turkish)
# 11. NEW: Natural language summary
# 12. NEW: Contextual warnings
# 13. NEW: Statistical glossary
#
# Author: ClinicoPath Development Team
# Date: 2024-01-15
# =============================================================================

# Load required packages
library(ClinicoPath)
library(dplyr)

# Load test data
data_path <- here::here("data", "ihc_breast_cancer.csv")
ihc_data <- read.csv(data_path, stringsAsFactors = TRUE)

cat("=============================================================================\n")
cat("IHC CLUSTERING COMPREHENSIVE FEATURE TESTS\n")
cat("=============================================================================\n\n")

cat(sprintf("Test dataset loaded: %d cases, %d variables\n", nrow(ihc_data), ncol(ihc_data)))
cat(sprintf("Ground truth subtypes: %s\n\n",
    paste(table(ihc_data$TrueSubtype), collapse=", ")))

# =============================================================================
# TEST 1: BASIC PAM CLUSTERING WITH AUTO K-SELECTION
# =============================================================================

cat("TEST 1: Basic PAM clustering with automatic k-selection\n")
cat("--------------------------------------------------------\n")

test1_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    autoSelectK = TRUE,
    kRange = "medium",
    scaleContVars = TRUE,
    handleMissing = "pairwise"
)

cat("Parameters:\n")
print(test1_data)

cat("\nExpected: 3-4 clusters identified automatically\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 2: HIERARCHICAL CLUSTERING WITH DENDROGRAM
# =============================================================================

cat("TEST 2: Hierarchical clustering with dendrogram\n")
cat("------------------------------------------------\n")

test2_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC", "CK5_6"),
    contVars = c("Ki67_Percent", "AR_Hscore"),
    method = "hierarchical",
    nClusters = 4,
    autoSelectK = FALSE,
    showDendrogram = TRUE,
    scaleContVars = TRUE
)

cat("Parameters:\n")
print(test2_data)

cat("\nExpected: Dendrogram showing 4 clusters\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 3: DIMENSION REDUCTION (MCA/PCA) WITH MANY MARKERS
# =============================================================================

cat("TEST 3: Dimension reduction clustering (many markers)\n")
cat("------------------------------------------------------\n")

test3_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC", "CK5_6", "EGFR"),
    contVars = c("Ki67_Percent", "AR_Hscore", "p53_Percent"),
    method = "dimreduce",
    autoSelectK = TRUE,
    showPCAPlot = TRUE,
    showHeatmap = TRUE,
    scaleContVars = TRUE
)

cat("Parameters:\n")
print(test3_data)

cat("\nExpected: PCA/MCA plot with cluster overlay\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 4: MISSING DATA STRATEGIES COMPARISON
# =============================================================================

cat("TEST 4: Missing data handling strategies\n")
cat("------------------------------------------\n")

cat("Test 4a: Complete cases only\n")
test4a_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = c("Ki67_Percent", "AR_Hscore"),
    method = "pam",
    handleMissing = "complete",
    nClusters = 3
)
cat("Parameters:\n")
print(test4a_data)
cat(sprintf("Expected: Analysis uses only complete cases (n=%d of %d)\n",
    sum(complete.cases(ihc_data[, c("ER_Status", "PR_Status", "HER2_IHC", "Ki67_Percent", "AR_Hscore")])),
    nrow(ihc_data)))

cat("\nTest 4b: Pairwise distances (retain all cases)\n")
test4b_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = c("Ki67_Percent", "AR_Hscore"),
    method = "pam",
    handleMissing = "pairwise",
    nClusters = 3
)
cat("Parameters:\n")
print(test4b_data)
cat(sprintf("Expected: Analysis uses all cases (n=%d) with pairwise distances\n", nrow(ihc_data)))
cat("✅ Test configurations ready for jamovi\n\n")

# =============================================================================
# TEST 5: CONSENSUS CLUSTERING FOR STABILITY
# =============================================================================

cat("TEST 5: Consensus clustering (bootstrap stability)\n")
cat("---------------------------------------------------\n")

test5_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    consensusClustering = TRUE,
    nBootstrap = 100,
    seed = 42
)

cat("Parameters:\n")
print(test5_data)

cat("\nExpected: Consensus stability table showing cluster reliability\n")
cat("Note: This takes longer due to 100 bootstrap iterations\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 6: CLINICAL CORRELATIONS
# =============================================================================

cat("TEST 6: Clinical variable correlations\n")
cat("----------------------------------------\n")

test6_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    clinicalVars = c("Age_Years", "Tumor_Grade", "Tumor_Stage", "Lymph_Node_Status")
)

cat("Parameters:\n")
print(test6_data)

cat("\nExpected: Clinical comparison table with chi-square/Kruskal-Wallis tests\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 7: SURVIVAL ANALYSIS BY CLUSTER
# =============================================================================

cat("TEST 7: Survival analysis integration\n")
cat("--------------------------------------\n")

test7_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    survivalTime = "OS_Months",
    survivalEvent = "OS_Event"
)

cat("Parameters:\n")
print(test7_data)

cat("\nExpected: Kaplan-Meier survival curves by cluster\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 8: ACCESSIBILITY FEATURES
# =============================================================================

cat("TEST 8: Accessibility features\n")
cat("-------------------------------\n")

cat("Test 8a: Colorblind-safe palette\n")
test8a_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    colorPalette = "colorblind",
    fontSize = "large",
    showSilhouette = TRUE,
    showHeatmap = TRUE
)
cat("Parameters:\n")
print(test8a_data)
cat("Expected: Plots use Wong colorblind-safe palette with 14pt font\n\n")

cat("Test 8b: High contrast mode\n")
test8b_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    colorPalette = "high_contrast",
    fontSize = "extra_large",
    plotContrast = TRUE
)
cat("Parameters:\n")
print(test8b_data)
cat("Expected: High-contrast plots with 16pt font and bold gridlines\n")
cat("✅ Test configurations ready for jamovi\n\n")

# =============================================================================
# TEST 9: TUMOR-SPECIFIC PRESETS
# =============================================================================

cat("TEST 9: Tumor-specific presets\n")
cat("-------------------------------\n")

test9_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    tumorPreset = "breast_luminal",
    applyPreset = TRUE
)

cat("Parameters:\n")
print(test9_data)

cat("\nExpected: Preset recommendations appear for breast luminal classification\n")
cat("Recommended settings: PAM method, 4 clusters, specific marker set\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 10: INTERNATIONALIZATION (TURKISH)
# =============================================================================

cat("TEST 10: Turkish language interface\n")
cat("------------------------------------\n")

test10_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    language = "turkish"
)

cat("Parameters:\n")
print(test10_data)

cat("\nExpected: All UI text, executive summary, and reports in Turkish\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 11: NATURAL LANGUAGE SUMMARY (NEW FEATURE)
# =============================================================================

cat("TEST 11: Natural language summary (NEW)\n")
cat("----------------------------------------\n")

test11_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    showNaturalSummary = TRUE,
    language = "english"
)

cat("Parameters:\n")
print(test11_data)

cat("\nExpected: Plain-language summary panel with:\n")
cat("  - Simple explanation of what was found\n")
cat("  - Cluster sizes in everyday language\n")
cat("  - Clinical interpretation\n")
cat("  - Recommended next steps\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 12: CONTEXTUAL WARNINGS (NEW FEATURE)
# =============================================================================

cat("TEST 12: Contextual warnings (NEW)\n")
cat("-----------------------------------\n")

cat("Test 12a: Warning for small clusters\n")
test12a_data <- data.frame(
    catVars = c("ER_Status", "PR_Status"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 8,  # Too many for 155 cases - will trigger warnings
    showWarnings = TRUE
)
cat("Parameters:\n")
print(test12a_data)
cat("Expected: Warning about overfitting and small cluster sizes\n\n")

cat("Test 12b: Warning for imbalanced clusters\n")
# Use subset to create imbalance
cat("Using subset with extreme marker values to create imbalance\n")
test12b_data <- data.frame(
    catVars = c("ER_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 3,
    showWarnings = TRUE
)
cat("Parameters:\n")
print(test12b_data)
cat("Expected: Warning about cluster imbalance if detected\n")
cat("✅ Test configurations ready for jamovi\n\n")

# =============================================================================
# TEST 13: STATISTICAL GLOSSARY (NEW FEATURE)
# =============================================================================

cat("TEST 13: Statistical glossary (NEW)\n")
cat("------------------------------------\n")

cat("Test 13a: English glossary\n")
test13a_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    showGlossary = TRUE,
    language = "english"
)
cat("Parameters:\n")
print(test13a_data)
cat("Expected: Glossary panel with definitions of:\n")
cat("  - Gower Distance, Silhouette Width, PAM\n")
cat("  - Hierarchical Clustering, Dimension Reduction\n")
cat("  - Consensus Clustering, Medoid, Effect sizes\n\n")

cat("Test 13b: Turkish glossary\n")
test13b_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    showGlossary = TRUE,
    language = "turkish"
)
cat("Parameters:\n")
print(test13b_data)
cat("Expected: Glossary panel with Turkish translations of statistical terms\n")
cat("✅ Test configurations ready for jamovi\n\n")

# =============================================================================
# TEST 14: CASE ID TRACKING
# =============================================================================

cat("TEST 14: Case ID tracking with medoid display\n")
cat("----------------------------------------------\n")

test14_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    caseId = "PatientID",
    method = "pam",
    nClusters = 4
)

cat("Parameters:\n")
print(test14_data)

cat("\nExpected: Medoid table shows actual Patient IDs (e.g., BC-0042)\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 15: CLUSTER EXPORT
# =============================================================================

cat("TEST 15: Cluster assignment export\n")
cat("-----------------------------------\n")

test15_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC"),
    contVars = "Ki67_Percent",
    method = "pam",
    nClusters = 4,
    exportClusters = TRUE
)

cat("Parameters:\n")
print(test15_data)

cat("\nExpected: Cluster assignments stored and notification displayed\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# TEST 16: COMPREHENSIVE ALL FEATURES
# =============================================================================

cat("TEST 16: Comprehensive test with all features enabled\n")
cat("------------------------------------------------------\n")

test16_data <- data.frame(
    catVars = c("ER_Status", "PR_Status", "HER2_IHC", "CK5_6", "EGFR"),
    contVars = c("Ki67_Percent", "AR_Hscore", "p53_Percent"),
    caseId = "PatientID",
    method = "pam",
    nClusters = 4,
    autoSelectK = FALSE,
    scaleContVars = TRUE,
    handleMissing = "pairwise",

    # Visualizations
    showSilhouette = TRUE,
    showHeatmap = TRUE,
    showPCAPlot = TRUE,
    showBoxplots = TRUE,
    heatmapScale = "row",

    # Output tables
    markerSummary = TRUE,
    clusterProfiles = TRUE,
    associationTests = TRUE,
    exportClusters = TRUE,

    # Clinical correlations
    clinicalVars = c("Age_Years", "Tumor_Grade", "Tumor_Stage"),
    survivalTime = "OS_Months",
    survivalEvent = "OS_Event",

    # Accessibility
    colorPalette = "colorblind",
    fontSize = "medium",
    plotContrast = FALSE,

    # NEW FEATURES
    showNaturalSummary = TRUE,
    showWarnings = TRUE,
    showGlossary = TRUE,

    # Internationalization
    language = "english"
)

cat("Parameters:\n")
print(test16_data)

cat("\nExpected: Comprehensive analysis with all features displayed:\n")
cat("  - 4 clusters identified\n")
cat("  - Multiple visualizations (silhouette, heatmap, PCA, boxplots)\n")
cat("  - Summary tables (marker summary, profiles, associations)\n")
cat("  - Clinical correlations and survival curves\n")
cat("  - Plain-language summary\n")
cat("  - Contextual warnings (if any issues detected)\n")
cat("  - Statistical glossary\n")
cat("  - Colorblind-safe palettes\n")
cat("✅ Test configuration ready for jamovi\n\n")

# =============================================================================
# SUMMARY AND RECOMMENDATIONS
# =============================================================================

cat("=============================================================================\n")
cat("TESTING SUMMARY\n")
cat("=============================================================================\n\n")

cat("Total test scenarios: 16\n\n")

cat("Test Coverage:\n")
cat("  ✅ Basic functionality (Tests 1-3)\n")
cat("  ✅ Missing data handling (Test 4)\n")
cat("  ✅ Advanced features (Tests 5-7)\n")
cat("  ✅ Accessibility (Test 8)\n")
cat("  ✅ Clinical presets (Test 9)\n")
cat("  ✅ Internationalization (Test 10)\n")
cat("  ✅ NEW: Natural language summaries (Test 11)\n")
cat("  ✅ NEW: Contextual warnings (Test 12)\n")
cat("  ✅ NEW: Statistical glossary (Test 13)\n")
cat("  ✅ Case tracking (Test 14)\n")
cat("  ✅ Export functionality (Test 15)\n")
cat("  ✅ Comprehensive integration (Test 16)\n\n")

cat("HOW TO USE THIS TEST SCRIPT:\n")
cat("-----------------------------\n")
cat("1. Load data in jamovi: File → Open → data/ihc_breast_cancer.csv\n")
cat("2. Open Analyses → OncoPathT → IHC Clustering Analysis\n")
cat("3. For each test, configure options as shown in test parameters\n")
cat("4. Verify expected outputs appear correctly\n")
cat("5. Check that new features (summary, warnings, glossary) work\n\n")

cat("VALIDATION CHECKLIST:\n")
cat("---------------------\n")
cat("[ ] Test 1: Auto k-selection identifies 3-4 clusters\n")
cat("[ ] Test 2: Dendrogram displays for hierarchical method\n")
cat("[ ] Test 3: PCA/MCA plot shows cluster separation\n")
cat("[ ] Test 4a: Complete cases analysis drops missing data\n")
cat("[ ] Test 4b: Pairwise analysis retains all cases\n")
cat("[ ] Test 5: Consensus statistics show stability metrics\n")
cat("[ ] Test 6: Clinical comparison table populated\n")
cat("[ ] Test 7: Survival plot displays with log-rank test\n")
cat("[ ] Test 8a: Colorblind palette applied (Wong colors)\n")
cat("[ ] Test 8b: High contrast mode visible\n")
cat("[ ] Test 9: Preset recommendations appear\n")
cat("[ ] Test 10: Turkish translations work correctly\n")
cat("[ ] Test 11: Natural language summary appears (NEW)\n")
cat("[ ] Test 12: Warnings display when issues detected (NEW)\n")
cat("[ ] Test 13: Glossary panel shows term definitions (NEW)\n")
cat("[ ] Test 14: Patient IDs appear in medoid table\n")
cat("[ ] Test 15: Cluster export notification appears\n")
cat("[ ] Test 16: All features work together\n\n")

cat("EXPECTED CLUSTERING RESULTS:\n")
cat("----------------------------\n")
cat("With proper settings, clusters should roughly correspond to:\n")
cat("  Cluster 1: Luminal A (ER+/PR+/HER2-/low Ki67) - ~60 cases\n")
cat("  Cluster 2: Luminal B (ER+/PR+/high Ki67 or HER2+) - ~40 cases\n")
cat("  Cluster 3: HER2-enriched (ER-/PR-/HER2 2+/3+) - ~25 cases\n")
cat("  Cluster 4: Triple Negative (ER-/PR-/HER2-) - ~25 cases\n\n")

cat("Compare cluster assignments with 'TrueSubtype' variable to validate\n")
cat("clustering quality (though some mixing is expected/realistic).\n\n")

cat("=============================================================================\n")
cat("TESTING COMPLETE - Ready for jamovi validation\n")
cat("=============================================================================\n")