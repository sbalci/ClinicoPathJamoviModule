# Diagnostic Style Clustering Example
# Demonstrates the Usubutun et al. (2012) methodology for identifying pathologist diagnostic "schools"

# Load required libraries
devtools::load_all()
library(ggplot2)
library(dplyr)

# =============================================================================
# Example 1: Endometrial Pathology (Original Usubutun Study Design)
# =============================================================================

# Load the endometrial diagnostic styles dataset
data("endometrial_diagnostic_styles")
endometrial_data <- endometrial_diagnostic_styles$diagnosis_data
pathologist_info <- endometrial_diagnostic_styles$pathologist_info

# Basic data exploration
cat("Dataset Overview:\n")
cat("Cases:", nrow(endometrial_data), "\n")
cat("Pathologists:", ncol(endometrial_data) - 6, "\n")  # Subtract case info columns
cat("Diagnostic Categories:", length(unique(unlist(endometrial_data[, 7:21]))), "\n")

# View first few cases
head(endometrial_data[, 1:10])  # First 6 columns + first 4 pathologists

# View pathologist characteristics
print(pathologist_info)

# =============================================================================
# Analysis with ClinicoPath agreement function
# =============================================================================

# Prepare data for analysis
rater_columns <- paste0("Path_", sprintf("%02d", 1:15))

# In jamovi, you would:
# 1. Load endometrial_diagnostic_styles.csv
# 2. Go to meddecide -> Agreement -> Interrater Reliability
# 3. Move Path_01 through Path_15 to "Raters/Observers"
# 4. Enable "Diagnostic Style Clustering (Usubutun Method)"
# 5. Set pathologist characteristics variables

# =============================================================================
# Expected Results Interpretation
# =============================================================================

cat("\n=== EXPECTED ANALYSIS RESULTS ===\n")

cat("\n1. OVERALL AGREEMENT:\n")
cat("   - Fleiss' Kappa: 0.65-0.75 (substantial agreement)\n")
cat("   - Overall Agreement: 68-78%\n")
cat("   - Individual accuracies: 60-85% (varies by experience)\n")

cat("\n2. DIAGNOSTIC STYLE GROUPS:\n")
cat("   Style 1 (Conservative Academic):\n")
cat("   - Members: Path_01, Path_02, Path_04, Path_05 (Harvard/Johns Hopkins trained)\n")
cat("   - Tendency: Conservative, less likely to diagnose EIN\n")
cat("   - Within-group agreement: 82-86%\n")
cat("   - Characteristics: Senior academics, gynecologic specialists\n")

cat("\n   Style 2 (Moderate Community):\n") 
cat("   - Members: Path_07, Path_08, Path_10, Path_11 (Community trained)\n")
cat("   - Tendency: Moderate diagnostic approach\n")
cat("   - Within-group agreement: 75-80%\n")
cat("   - Characteristics: General pathologists, community practice\n")

cat("\n   Style 3 (Aggressive International):\n")
cat("   - Members: Path_13, Path_14, Path_15 (European/Asian trained)\n")
cat("   - Tendency: More aggressive, higher EIN diagnosis rate\n")
cat("   - Within-group agreement: 85-90%\n")
cat("   - Characteristics: International training, research institutions\n")

cat("\n3. DISCORDANT CASES:\n")
cat("   - Cases with polyps: High inter-style disagreement\n")
cat("   - Hormonal effect cases: Conservative vs aggressive split\n")
cat("   - Poor quality cases: Random disagreement patterns\n")

# =============================================================================
# Example 2: Breast Pathology Diagnostic Styles
# =============================================================================

cat("\n\n=== BREAST PATHOLOGY EXAMPLE ===\n")

# Load breast diagnostic data
data("breast_diagnostic_styles")
breast_data <- breast_diagnostic_styles$diagnosis_data
breast_pathologist_info <- breast_diagnostic_styles$pathologist_info

cat("Breast Dataset Overview:\n")
cat("Cases:", nrow(breast_data), "\n")
cat("Pathologists:", ncol(breast_data) - 5, "\n")
cat("Categories: Benign, Atypical, DCIS, Invasive\n")

# View pathologist characteristics
print(breast_pathologist_info)

cat("\nExpected Breast Style Groups:\n")
cat("Style 1 (Conservative): Tend to undercall atypical lesions\n")
cat("Style 2 (Moderate): Balanced diagnostic approach\n") 
cat("Style 3 (Aggressive): More liberal with atypia/DCIS diagnosis\n")

# =============================================================================
# Example 3: Lymphoma Classification Styles  
# =============================================================================

cat("\n\n=== LYMPHOMA CLASSIFICATION EXAMPLE ===\n")

# Load lymphoma data
data("lymphoma_diagnostic_styles")
lymphoma_data <- lymphoma_diagnostic_styles$diagnosis_data
lymphoma_pathologist_info <- lymphoma_diagnostic_styles$pathologist_info

cat("Lymphoma Dataset Overview:\n")
cat("Cases:", nrow(lymphoma_data), "\n")
cat("Pathologists:", ncol(lymphoma_data) - 5, "\n")
cat("Categories: Reactive, DLBCL, Follicular, Marginal Zone, Mantle Cell\n")

print(lymphoma_pathologist_info)

cat("\nExpected Lymphoma Style Groups:\n")
cat("Style 1 (Traditional Morphologists): Heavy reliance on histology\n")
cat("Style 2 (Molecular-Integrated): Emphasize genetic findings\n")
cat("Style 3 (Clinical-Context): Consider presentation heavily\n")

# =============================================================================
# Visualization Examples (what users will see in jamovi)
# =============================================================================

cat("\n\n=== VISUALIZATION OUTPUTS ===\n")

cat("\n1. DIAGNOSTIC STYLE DENDROGRAM:\n")
cat("   - Hierarchical tree showing pathologist clustering\n")
cat("   - Branch heights indicate diagnostic similarity\n")
cat("   - Clear separation between style groups\n")
cat("   - Pathologists cluster by diagnostic approach, not alphabetically\n")

cat("\n2. DIAGNOSTIC STYLE HEATMAP:\n")
cat("   - Rows: Pathologists (grouped by style)\n")
cat("   - Columns: Diagnostic categories\n")
cat("   - Colors: Frequency of diagnosis usage\n")
cat("   - Patterns: Similar colors within style groups\n")

# =============================================================================
# Clinical Action Items Based on Results
# =============================================================================

cat("\n\n=== CLINICAL ACTION ITEMS ===\n")

cat("\n1. QUALITY ASSURANCE:\n")
cat("   - Monitor pathologists with unusual diagnostic patterns\n")
cat("   - Implement peer consultation for discordant case types\n")
cat("   - Develop institution-specific diagnostic guidelines\n")

cat("\n2. TRAINING PROGRAMS:\n")
cat("   - Identify mentor influence on diagnostic style\n")
cat("   - Standardize training across different 'schools'\n")
cat("   - Focus education on high-discord case types\n")

cat("\n3. CONSENSUS DEVELOPMENT:\n")
cat("   - Include representatives from each diagnostic style\n")
cat("   - Address style-specific biases in guidelines\n")
cat("   - Create educational materials for discordant cases\n")

# =============================================================================
# Statistical Validation
# =============================================================================

cat("\n\n=== STATISTICAL VALIDATION ===\n")

cat("\nKey Metrics to Validate Results:\n")
cat("1. Within-group agreement should be >75%\n")
cat("2. Style groups should correlate with characteristics (p<0.05)\n") 
cat("3. Discordant cases should show systematic patterns\n")
cat("4. Dendrogram should show clear branch separation\n")
cat("5. Heatmap should reveal distinct color patterns by group\n")

# =============================================================================
# Usage Notes
# =============================================================================

cat("\n\n=== USAGE NOTES ===\n")

cat("\nMinimum Requirements:\n")
cat("- At least 6-8 pathologists for meaningful clustering\n")
cat("- 30-50 cases minimum for stable patterns\n")
cat("- Include range of case difficulties\n")
cat("- Collect pathologist characteristic data\n")

cat("\nOptimal Settings:\n")
cat("- 3 style groups (Usubutun standard)\n")
cat("- Ward's linkage clustering method\n")
cat("- Percentage agreement distance metric\n")
cat("- Include rater characteristics analysis\n")
cat("- Enable discordant case identification\n")

cat("\nInterpretation Guidelines:\n")
cat("- High within-group agreement (>80%) = strong style membership\n")
cat("- Style groups should correlate with training/experience\n")
cat("- Discordant cases reveal diagnostic philosophy differences\n")
cat("- Results should make clinical sense\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Use these datasets in jamovi to explore diagnostic style clustering!\n")

cat("\n\n=== KEY REFERENCES ===\n")

cat("\nPrimary Methodology:\n")
cat("Usubutun A, Mutter GL, Saglam A, Dolgun A, Ozkan EA, Ince T, Akyol A, et al.\n")
cat("Reproducibility of endometrial intraepithelial neoplasia diagnosis is good,\n")
cat("but influenced by the diagnostic style of pathologists.\n")
cat("Modern Pathology. 2012;25(6):877-884. PMID: 22301705\n")

cat("\nRelated IHC Clustering Methods:\n")
cat("1. Sterlacci W, et al. Cluster Analysis According to Immunohistochemistry\n")
cat("   is a Robust Tool for Non-Small Cell Lung Cancer. Appl Immunohistochem\n")
cat("   Mol Morphol. 2020;28(4):274-283. PMID: 31058655\n")

cat("\n2. Olsen SH, et al. Cluster analysis of immunohistochemical profiles\n")
cat("   in synovial sarcoma, malignant peripheral nerve sheath tumor, and\n")
cat("   Ewing sarcoma. Mod Pathol. 2006;19(5):659-68. PMID: 16528378\n")

cat("\n3. Matsuoka T, et al. Cluster analysis of claudin-1 and -4, E-cadherin,\n")
cat("   and β-catenin expression in colorectal cancers.\n")
cat("   J Surg Oncol. 2011;103(7):674-86. PMID: 21360533\n")

cat("\n4. Carvalho JC, et al. Cluster analysis of immunohistochemical profiles\n")
cat("   delineates CK7, vimentin, S100A1 and C-kit (CD117) as an optimal panel\n")
cat("   in the differential diagnosis of renal oncocytoma from its mimics.\n")
cat("   Histopathology. 2011;58(2):169-79. PMID: 21323945\n")

cat("\n5. Laas E, et al. Unsupervised Clustering of Immunohistochemical Markers\n")
cat("   to Define High-Risk Endometrial Cancer.\n")
cat("   Pathol Oncol Res. 2019;25(2):461-469. PMID: 29264761\n")