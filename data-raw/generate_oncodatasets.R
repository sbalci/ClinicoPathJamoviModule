# Data Generation Script: OncoDataSets for ClinicoPath
# This script imports datasets from OncoDataSets package and re-exports them
# for use in ClinicoPath and its submodules

# Load required packages
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(OncoDataSets)
library(usethis)
library(devtools)

# Create data-raw directory if it doesn't exist
if (!dir.exists("data-raw")) {
  dir.create("data-raw")
}

# Function to safely load and save datasets
save_oncology_dataset <- function(dataset_name, description = NULL) {
  cat("Processing:", dataset_name, "\n")
  
  # Load the dataset
  tryCatch({
    data(list = dataset_name, package = "OncoDataSets", envir = environment())
    dataset <- get(dataset_name, envir = environment())
    
    # Create documentation if description provided
    if (!is.null(description)) {
      cat("  Description:", description, "\n")
    }
    
    # Save to data/ directory
    assign(dataset_name, dataset)
    save(list = dataset_name, file = paste0("data/", dataset_name, ".rda"), compress = "bzip2")
    
    cat("  ✓ Saved:", dataset_name, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("  ✗ Error saving", dataset_name, ":", e$message, "\n")
    return(FALSE)
  })
}

# Ensure data directory exists
if (!dir.exists("data")) {
  dir.create("data")
}

cat("=== Importing OncoDataSets for ClinicoPath ===\n\n")

# SURVIVAL ANALYSIS DATASETS (for jsurvival)
cat("--- Survival Analysis Datasets ---\n")
save_oncology_dataset("Melanoma_df", "Melanoma patient survival data with tumor characteristics")
save_oncology_dataset("LeukemiaSurvival_df", "Leukemia patient survival with treatment information")
save_oncology_dataset("ProstateSurvival_df", "Prostate cancer survival by grade, stage, and age")
save_oncology_dataset("NCCTGLungCancer_df", "NCCTG lung cancer trial data")
save_oncology_dataset("OvarianCancer_df", "Ovarian cancer trial survival data")

# DIAGNOSTIC/DECISION ANALYSIS DATASETS (for meddecide)
cat("\n--- Diagnostic and Decision Analysis Datasets ---\n")
save_oncology_dataset("PSAProstateCancer_df", "PSA levels and prostate cancer outcomes")
save_oncology_dataset("CA19PancreaticCancer_df", "CA19-9 diagnostic accuracy studies for pancreatic cancer")
save_oncology_dataset("LungNodulesDetected_df", "Lung nodule characteristics and malignancy outcomes")

# DESCRIPTIVE ANALYSIS DATASETS (for ClinicoPathDescriptives)
cat("\n--- Descriptive Analysis Datasets ---\n")
save_oncology_dataset("BreastCancerWI_df", "Wisconsin Breast Cancer diagnostic features")
save_oncology_dataset("ChildCancer_df", "Childhood cancer epidemiological data")
save_oncology_dataset("BladderCancer_df", "Bladder cancer patient characteristics")
save_oncology_dataset("SmokingLungCancer_df", "Smoking status and lung cancer relationship")
save_oncology_dataset("BrainCancerCases_df", "Brain cancer case characteristics")
save_oncology_dataset("BrainCancerGeo_df", "Brain cancer geographic distribution")

# VISUALIZATION DATASETS (for jjstatsplot)
cat("\n--- Additional Visualization Datasets ---\n")
save_oncology_dataset("ColonCancerChemo_df", "Colon cancer chemotherapy outcomes")
save_oncology_dataset("EndometrialCancer_df", "Endometrial cancer patient data")
save_oncology_dataset("HeadNeckCarcinoma_df", "Head and neck carcinoma characteristics") 
save_oncology_dataset("SkinCancerChemo_df", "Skin cancer chemotherapy response")

# BIOMARKER DATASETS
cat("\n--- Biomarker Analysis Datasets ---\n")
save_oncology_dataset("BRCA1BreastCancer_df", "BRCA1 mutations in breast cancer")
save_oncology_dataset("BRCA2BreastCancer_df", "BRCA2 mutations in breast cancer")
save_oncology_dataset("BRCA1OvarianCancer_df", "BRCA1 mutations in ovarian cancer")
save_oncology_dataset("BRCA2OvarianCancer_df", "BRCA2 mutations in ovarian cancer")
save_oncology_dataset("CASP8BreastCancer_df", "CASP8 gene variants in breast cancer")

# EPIDEMIOLOGICAL DATASETS
cat("\n--- Epidemiological Datasets ---\n")
save_oncology_dataset("USCancerStats_df", "US cancer statistics")
save_oncology_dataset("USMortalityCancer_df", "US cancer mortality data")
save_oncology_dataset("UKLungCancerDeaths_df", "UK lung cancer mortality")
save_oncology_dataset("CancerSmokeCity_array", "Cancer and smoking by city")

# SPECIALIZED DATASETS
cat("\n--- Specialized Analysis Datasets ---\n")
save_oncology_dataset("ColorectalMiRNAs_tbl_df", "Colorectal cancer microRNA data")
save_oncology_dataset("PancreaticMiRNAs_tbl_df", "Pancreatic cancer microRNA data")
save_oncology_dataset("SmallCellLung_tbl_df", "Small cell lung cancer data")
save_oncology_dataset("WBreastCancer_tbl_df", "Wisconsin breast cancer extended data")

# EXPERIMENTAL/RESEARCH DATASETS
cat("\n--- Experimental Research Datasets ---\n")
save_oncology_dataset("MaleMiceCancer_df", "Male mice cancer research data")
save_oncology_dataset("MiceDeathRadiation_df", "Mice radiation effect studies")
save_oncology_dataset("RadiationEffects_df", "Radiation treatment effects")

# CASE STUDIES AND CONTROLS
cat("\n--- Case-Control Studies ---\n")
save_oncology_dataset("LeukemiaLymphomaCases_df", "Leukemia and lymphoma cases")
save_oncology_dataset("LeukemiaLymphomaControl_df", "Leukemia and lymphoma controls")
save_oncology_dataset("LeukemiaLymphomaGeo_df", "Leukemia and lymphoma geographic data")
save_oncology_dataset("SuspectedCancer_df", "Suspected cancer case investigations")

# RISK FACTOR DATASETS
cat("\n--- Risk Factor Analysis ---\n")
save_oncology_dataset("AlcoholIntakeCancer_df", "Alcohol intake and cancer risk")
save_oncology_dataset("AflatoxinLiverCancer_df", "Aflatoxin exposure and liver cancer")
save_oncology_dataset("VinylideneLiverCancer_df", "Vinylidene chloride and liver cancer")

# TREATMENT AND OUTCOMES
cat("\n--- Treatment Outcomes ---\n")
save_oncology_dataset("LeukemiaRemission_df", "Leukemia remission data")
save_oncology_dataset("ProstateSurgery_df", "Prostate surgery outcomes")
save_oncology_dataset("BloodStorageProstate_df", "Blood storage and prostate cancer")
save_oncology_dataset("NodalProstate_df", "Nodal involvement in prostate cancer")
save_oncology_dataset("ProstateMethylation_df", "Prostate cancer methylation patterns")

# OTHER DATASETS
cat("\n--- Additional Specialized Datasets ---\n")
save_oncology_dataset("CervicalCancer_df", "Cervical cancer patient data")
save_oncology_dataset("Carcinoma_p53_df", "p53 mutations in carcinoma")
save_oncology_dataset("ICGCLiver_df", "ICGC liver cancer genomics")
save_oncology_dataset("LungCancerETS_df", "Lung cancer ETS exposure")
save_oncology_dataset("AIPulmonaryNodules_df", "AI-detected pulmonary nodules")
save_oncology_dataset("RotterdamBreastCancer_df", "Rotterdam breast cancer study")
save_oncology_dataset("USRegionalMortality_df", "US regional cancer mortality")
save_oncology_dataset("VALungCancer_list", "VA lung cancer study data")
save_oncology_dataset("cancer_in_dogs_tbl_df", "Cancer studies in canine models")

cat("\n=== Data Import Summary ===\n")
cat("Successfully imported oncology datasets from OncoDataSets package\n")
cat("Datasets are now available in ClinicoPath package data/ directory\n")
cat("Use data('dataset_name') to load any dataset in R or jamovi\n")

# Create a summary dataset listing all available datasets
oncology_datasets_summary <- data.frame(
  dataset_name = c(
    # Survival datasets
    "Melanoma_df", "LeukemiaSurvival_df", "ProstateSurvival_df", 
    "NCCTGLungCancer_df", "OvarianCancer_df",
    
    # Diagnostic datasets
    "PSAProstateCancer_df", "CA19PancreaticCancer_df", "LungNodulesDetected_df",
    
    # Descriptive datasets
    "BreastCancerWI_df", "ChildCancer_df", "BladderCancer_df", 
    "SmokingLungCancer_df", "BrainCancerCases_df", "BrainCancerGeo_df",
    
    # Additional datasets
    "ColonCancerChemo_df", "EndometrialCancer_df", "HeadNeckCarcinoma_df",
    "SkinCancerChemo_df", "BRCA1BreastCancer_df", "BRCA2BreastCancer_df",
    "BRCA1OvarianCancer_df", "BRCA2OvarianCancer_df", "CASP8BreastCancer_df",
    "USCancerStats_df", "USMortalityCancer_df", "UKLungCancerDeaths_df",
    "ColorectalMiRNAs_tbl_df", "PancreaticMiRNAs_tbl_df", "SmallCellLung_tbl_df",
    "WBreastCancer_tbl_df", "MaleMiceCancer_df", "MiceDeathRadiation_df",
    "RadiationEffects_df", "LeukemiaLymphomaCases_df", "LeukemiaLymphomaControl_df",
    "LeukemiaLymphomaGeo_df", "SuspectedCancer_df", "AlcoholIntakeCancer_df",
    "AflatoxinLiverCancer_df", "VinylideneLiverCancer_df", "LeukemiaRemission_df",
    "ProstateSurgery_df", "BloodStorageProstate_df", "NodalProstate_df",
    "ProstateMethylation_df", "CervicalCancer_df", "Carcinoma_p53_df",
    "ICGCLiver_df", "LungCancerETS_df", "AIPulmonaryNodules_df",
    "RotterdamBreastCancer_df", "USRegionalMortality_df", "VALungCancer_list",
    "CancerSmokeCity_array", "cancer_in_dogs_tbl_df"
  ),
  
  category = c(
    # Survival datasets
    rep("Survival Analysis", 5),
    
    # Diagnostic datasets  
    rep("Diagnostic/Decision Analysis", 3),
    
    # Descriptive datasets
    rep("Descriptive/Comparative Analysis", 6),
    
    # Additional categories
    rep("Treatment Outcomes", 4),
    rep("Biomarker Analysis", 5),
    rep("Epidemiological", 3),
    rep("Molecular/Genomic", 3),
    rep("Experimental", 3),
    rep("Case-Control Studies", 4),
    rep("Risk Factors", 3),
    rep("Clinical Outcomes", 5),
    rep("Specialized Studies", 8)
  ),
  
  primary_use = c(
    # Survival datasets
    "Kaplan-Meier, Cox regression", "Treatment comparison", "Stratified survival", 
    "Clinical trial analysis", "Small sample survival",
    
    # Diagnostic datasets
    "ROC analysis, biomarker evaluation", "Meta-analysis diagnostic accuracy", 
    "Decision trees, clinical rules",
    
    # Descriptive datasets
    "Classification, Table One", "Pediatric epidemiology", "Clinical characteristics",
    "Risk factor analysis", "Neurological oncology", "Geographic patterns",
    
    # Additional uses
    "Chemotherapy response", "Gynecologic oncology", "Head/neck cancer",
    "Dermatologic oncology", "Genetic testing", "Genetic testing",
    "Genetic counseling", "Genetic counseling", "Genetic variants",
    "Population statistics", "Mortality trends", "International comparison",
    "Biomarker discovery", "Biomarker discovery", "Rare tumors",
    "Feature comparison", "Animal models", "Radiation studies",
    "Treatment effects", "Case identification", "Control matching",
    "Spatial analysis", "Screening programs", "Environmental risk",
    "Environmental risk", "Environmental risk", "Long-term outcomes",
    "Surgical outcomes", "Biobanking studies", "Staging systems",
    "Methylation analysis", "Gynecologic screening", "Molecular pathology",
    "Genomic consortiums", "Occupational exposure", "AI/ML applications",
    "Population studies", "Regional variations", "Healthcare systems",
    "Environmental studies", "Comparative oncology"
  ),
  
  stringsAsFactors = FALSE
)

# Save the summary
save(oncology_datasets_summary, file = "data/oncology_datasets_summary.rda", compress = "bzip2")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(oncology_datasets_summary, "data/oncology_datasets_summary.omv")
  message("✓ Created oncology_datasets_summary.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(oncology_datasets_summary, "data/oncology_datasets_summary.omv")
  message("✓ Created oncology_datasets_summary.omv")
}

cat("\nCreated oncology_datasets_summary with dataset information\n")
cat("Use data('oncology_datasets_summary') to see all available datasets\n")

cat("\n=== Next Steps ===\n")
cat("1. Update DESCRIPTION file to add OncoDataSets to Suggests\n")
cat("2. Update documentation files in man/ directory\n") 
cat("3. Run devtools::document() to update package documentation\n")
cat("4. Consider adding dataset help files in R/ directory\n")

cat("\n=== Usage in Vignettes ===\n")
cat("In vignettes, use: data('dataset_name')\n")
cat("Example: data('Melanoma_df') # for survival analysis\n")
cat("Example: data('PSAProstateCancer_df') # for ROC analysis\n")

cat("\nData generation script completed successfully!\n")
