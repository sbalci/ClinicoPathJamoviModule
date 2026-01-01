# Enhanced Comprehensive IHC & Molecular Pathology Dataset Generation
# This script generates synthetic data covering traditional IHC plus modern molecular pathology:
# 1. Traditional IHC markers for tumor classification and prognosis
# 2. Molecular biomarkers (mutations, fusions, amplifications)
# 3. Predictive biomarkers for targeted therapy
# 4. Digital pathology integration (AI-assisted scoring)
# 5. Multi-platform validation scenarios
# 
# Educational Focus: Modern pathology practice combining morphology, 
# immunophenotyping, and molecular characterization

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(tibble)
library(magrittr)

set.seed(12345)  # For reproducible results

# Sample size for comprehensive testing
n_samples <- 400  # Increased for more robust testing

# Generate Patient IDs
patient_id <- paste0("MP", sprintf("%04d", 1:n_samples))

# =============================================================================
# CLINICAL AND DEMOGRAPHIC VARIABLES
# =============================================================================

# Patient demographics and clinical data
age <- round(rnorm(n_samples, mean = 65, sd = 12))
age[age < 18] <- 18 + sample(1:30, sum(age < 18), replace = TRUE)
age[age > 95] <- 95 - sample(1:10, sum(age > 95), replace = TRUE)

gender <- sample(c("Male", "Female"), n_samples, replace = TRUE, prob = c(0.45, 0.55))

# Institution and operator variables for multi-center validation
institution <- sample(c("Academic_Medical_Center", "Community_Hospital", 
                       "Cancer_Center", "Regional_Hospital"), 
                     n_samples, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))

pathologist_experience <- sample(c("Trainee", "Junior", "Senior", "Expert"), 
                               n_samples, replace = TRUE, prob = c(0.1, 0.3, 0.4, 0.2))

# =============================================================================
# TUMOR CLASSIFICATION AND STAGING
# =============================================================================

# Expanded tumor types for modern molecular pathology
tumor_types <- c(
  "Breast_Invasive_Ductal", "Breast_Invasive_Lobular", "Breast_Triple_Negative",
  "Lung_Adenocarcinoma", "Lung_Squamous_Cell", "Lung_Small_Cell",
  "Colorectal_Adenocarcinoma", "Gastric_Adenocarcinoma",
  "Melanoma", "Prostate_Adenocarcinoma", "Renal_Clear_Cell",
  "Glioblastoma", "Pancreatic_Ductal", "Ovarian_Serous",
  "Soft_Tissue_Sarcoma", "Lymphoma_DLBCL"
)

tumor_type <- sample(tumor_types, n_samples, replace = TRUE)

# WHO/AJCC staging
t_stage <- sample(c("T1", "T2", "T3", "T4"), n_samples, replace = TRUE, 
                 prob = c(0.3, 0.35, 0.25, 0.1))
n_stage <- sample(c("N0", "N1", "N2", "N3"), n_samples, replace = TRUE,
                 prob = c(0.5, 0.3, 0.15, 0.05))
m_stage <- sample(c("M0", "M1"), n_samples, replace = TRUE, prob = c(0.85, 0.15))

grade <- sample(1:3, n_samples, replace = TRUE, prob = c(0.25, 0.45, 0.3))

# =============================================================================
# MOLECULAR BIOMARKERS - MUTATIONS AND FUSIONS
# =============================================================================

# Generate molecular alterations based on tumor type
generate_mutation_status <- function(tumor_type, mutation, base_rate = 0.1) {
  # Tumor-specific mutation rates
  rates <- case_when(
    tumor_type == "Lung_Adenocarcinoma" & mutation == "EGFR" ~ 0.15,
    tumor_type == "Lung_Adenocarcinoma" & mutation == "KRAS" ~ 0.30,
    tumor_type == "Lung_Adenocarcinoma" & mutation == "ALK_fusion" ~ 0.05,
    tumor_type == "Breast_Invasive_Ductal" & mutation == "PIK3CA" ~ 0.35,
    tumor_type == "Breast_Triple_Negative" & mutation == "BRCA1" ~ 0.20,
    tumor_type == "Colorectal_Adenocarcinoma" & mutation == "KRAS" ~ 0.45,
    tumor_type == "Colorectal_Adenocarcinoma" & mutation == "BRAF" ~ 0.10,
    tumor_type == "Melanoma" & mutation == "BRAF" ~ 0.45,
    tumor_type == "Glioblastoma" & mutation == "IDH1" ~ 0.08,
    tumor_type == "Glioblastoma" & mutation == "MGMT_methylation" ~ 0.40,
    TRUE ~ base_rate
  )
  
  # Generate result for each sample
  results <- character(length(tumor_type))
  for (i in seq_along(tumor_type)) {
    results[i] <- sample(c("Positive", "Negative"), 1, 
                        prob = c(rates[i], 1 - rates[i]))
  }
  return(results)
}

# Major oncogenic mutations
egfr_mutation <- generate_mutation_status(tumor_type, "EGFR")
kras_mutation <- generate_mutation_status(tumor_type, "KRAS")
braf_mutation <- generate_mutation_status(tumor_type, "BRAF")
pik3ca_mutation <- generate_mutation_status(tumor_type, "PIK3CA")
brca1_mutation <- generate_mutation_status(tumor_type, "BRCA1")
brca2_mutation <- generate_mutation_status(tumor_type, "BRCA2")

# Fusion genes
alk_fusion <- generate_mutation_status(tumor_type, "ALK_fusion")
ros1_fusion <- generate_mutation_status(tumor_type, "ROS1_fusion", 0.02)
ret_fusion <- generate_mutation_status(tumor_type, "RET_fusion", 0.01)

# Epigenetic markers
idh1_mutation <- generate_mutation_status(tumor_type, "IDH1")
mgmt_methylation <- generate_mutation_status(tumor_type, "MGMT_methylation")

# Microsatellite instability
msi_status <- sample(c("MSI-High", "MSI-Low", "MSS"), n_samples, replace = TRUE,
                    prob = c(0.15, 0.10, 0.75))

# Tumor mutational burden (mutations per megabase)
tmb <- round(rlnorm(n_samples, meanlog = 2, sdlog = 1))
tmb[tmb > 50] <- 50  # Cap at 50 mutations/Mb

# =============================================================================
# PREDICTIVE BIOMARKERS FOR TARGETED THERAPY
# =============================================================================

# HER2 testing (IHC + FISH)
her2_ihc <- sample(c("0", "1+", "2+", "3+"), n_samples, replace = TRUE,
                  prob = c(0.50, 0.25, 0.15, 0.10))

# FISH amplification (only relevant for 2+ and 3+ cases)
her2_fish_ratio <- ifelse(her2_ihc %in% c("2+", "3+"),
                         rnorm(sum(her2_ihc %in% c("2+", "3+")), 
                               mean = ifelse(her2_ihc[her2_ihc %in% c("2+", "3+")] == "3+", 3.5, 1.8),
                               sd = 0.8),
                         NA)

her2_fish_status <- case_when(
  is.na(her2_fish_ratio) ~ "Not_Performed",
  her2_fish_ratio >= 2.0 ~ "Amplified",
  TRUE ~ "Not_Amplified"
)

# PD-L1 expression (multiple scoring systems)
pdl1_tps <- round(runif(n_samples, min = 0, max = 100))  # Tumor Proportion Score
pdl1_cps <- round(runif(n_samples, min = 0, max = 100))  # Combined Positive Score
pdl1_ic <- round(runif(n_samples, min = 0, max = 100))   # Immune Cell Score

# Estrogen and Progesterone receptors (breast cancer)
er_percentage <- ifelse(tumor_type %in% c("Breast_Invasive_Ductal", "Breast_Invasive_Lobular"),
                       round(runif(sum(tumor_type %in% c("Breast_Invasive_Ductal", "Breast_Invasive_Lobular")), 
                                  min = 0, max = 100)), NA)

pr_percentage <- ifelse(tumor_type %in% c("Breast_Invasive_Ductal", "Breast_Invasive_Lobular"),
                       round(runif(sum(tumor_type %in% c("Breast_Invasive_Ductal", "Breast_Invasive_Lobular")), 
                                  min = 0, max = 100)), NA)

# =============================================================================
# TRADITIONAL IHC MARKERS WITH DIGITAL PATHOLOGY INTEGRATION
# =============================================================================

# Ki-67 proliferation index with both manual and AI-assisted scoring
ki67_manual <- round(runif(n_samples, min = 1, max = 90))
ki67_ai_assisted <- ki67_manual + round(rnorm(n_samples, mean = 0, sd = 3))
ki67_ai_assisted[ki67_ai_assisted < 0] <- 0
ki67_ai_assisted[ki67_ai_assisted > 100] <- 100

# Agreement between manual and AI scoring
ki67_agreement <- abs(ki67_manual - ki67_ai_assisted) <= 5

# p53 with mutation correlation
p53_ihc <- sample(c("Wild_type_pattern", "Null_pattern", "Overexpression"), 
                 n_samples, replace = TRUE, prob = c(0.6, 0.2, 0.2))

# CD8+ T-cell infiltration (cells per mm²)
cd8_count <- round(rlnorm(n_samples, meanlog = 4, sdlog = 1))

# Tumor-infiltrating lymphocytes percentage
til_percentage <- round(runif(n_samples, min = 0, max = 80))

# =============================================================================
# MULTI-PLATFORM VALIDATION DATA
# =============================================================================

# Same biomarker tested on different platforms
platform_a_pdl1 <- pdl1_tps
platform_b_pdl1 <- platform_a_pdl1 + round(rnorm(n_samples, mean = 0, sd = 5))
platform_b_pdl1[platform_b_pdl1 < 0] <- 0
platform_b_pdl1[platform_b_pdl1 > 100] <- 100

# Cross-platform agreement
platform_agreement <- abs(platform_a_pdl1 - platform_b_pdl1) <= 10

# Quality control metrics
specimen_adequacy <- sample(c("Adequate", "Suboptimal", "Inadequate"), 
                          n_samples, replace = TRUE, prob = c(0.85, 0.12, 0.03))

fixation_time <- round(rnorm(n_samples, mean = 8, sd = 3))  # Hours in formalin
fixation_time[fixation_time < 2] <- 2
fixation_time[fixation_time > 24] <- 24

# =============================================================================
# CLINICAL OUTCOMES AND TREATMENT RESPONSE
# =============================================================================

# Overall survival with molecular correlation
baseline_hazard <- 0.05
molecular_hr <- case_when(
  egfr_mutation == "Positive" ~ 0.7,  # Favorable with targeted therapy
  kras_mutation == "Positive" ~ 1.3,  # Worse prognosis
  braf_mutation == "Positive" & tumor_type == "Melanoma" ~ 0.6,  # Targeted therapy benefit
  msi_status == "MSI-High" ~ 0.5,     # Immunotherapy benefit
  TRUE ~ 1.0
)

# Generate survival times
survival_months <- round(rexp(n_samples, rate = baseline_hazard * molecular_hr))
survival_months[survival_months > 120] <- 120  # Cap at 10 years

death_event <- rbinom(n_samples, 1, 0.4)

# Progression-free survival
pfs_months <- round(survival_months * runif(n_samples, min = 0.3, max = 0.9))
progression_event <- rbinom(n_samples, 1, 0.6)

# Treatment response (RECIST criteria)
treatment_response <- sample(c("Complete_Response", "Partial_Response", 
                             "Stable_Disease", "Progressive_Disease"),
                           n_samples, replace = TRUE, prob = c(0.05, 0.25, 0.35, 0.35))

# =============================================================================
# QUALITY METRICS AND REPORTING
# =============================================================================

# Turnaround time for molecular testing
molecular_tat_days <- round(rnorm(n_samples, mean = 7, sd = 2))
molecular_tat_days[molecular_tat_days < 3] <- 3

# Reporting pathologist confidence
diagnostic_confidence <- sample(c("High", "Moderate", "Low"), 
                              n_samples, replace = TRUE, prob = c(0.7, 0.25, 0.05))

# External quality assessment scores
eqa_score <- round(rnorm(n_samples, mean = 85, sd = 10))
eqa_score[eqa_score < 60] <- 60
eqa_score[eqa_score > 100] <- 100

# =============================================================================
# CREATE COMPREHENSIVE DATASET
# =============================================================================

ihc_molecular_comprehensive <- tibble(
  Patient_ID = patient_id,
  Age = age,
  Gender = gender,
  Institution = institution,
  Pathologist_Experience = pathologist_experience,
  
  # Tumor characteristics
  Tumor_Type = tumor_type,
  T_Stage = t_stage,
  N_Stage = n_stage,
  M_Stage = m_stage,
  Grade = grade,
  
  # Molecular alterations
  EGFR_Mutation = egfr_mutation,
  KRAS_Mutation = kras_mutation,
  BRAF_Mutation = braf_mutation,
  PIK3CA_Mutation = pik3ca_mutation,
  BRCA1_Mutation = brca1_mutation,
  BRCA2_Mutation = brca2_mutation,
  
  # Fusion genes
  ALK_Fusion = alk_fusion,
  ROS1_Fusion = ros1_fusion,
  RET_Fusion = ret_fusion,
  
  # Epigenetic and genomic instability
  IDH1_Mutation = idh1_mutation,
  MGMT_Methylation = mgmt_methylation,
  MSI_Status = msi_status,
  Tumor_Mutational_Burden = tmb,
  
  # Predictive biomarkers
  HER2_IHC = her2_ihc,
  HER2_FISH_Ratio = her2_fish_ratio,
  HER2_FISH_Status = her2_fish_status,
  PD_L1_TPS = pdl1_tps,
  PD_L1_CPS = pdl1_cps,
  PD_L1_IC = pdl1_ic,
  ER_Percentage = er_percentage,
  PR_Percentage = pr_percentage,
  
  # Traditional IHC with digital integration
  Ki67_Manual = ki67_manual,
  Ki67_AI_Assisted = ki67_ai_assisted,
  Ki67_Agreement = ki67_agreement,
  p53_Pattern = p53_ihc,
  CD8_Count_per_mm2 = cd8_count,
  TIL_Percentage = til_percentage,
  
  # Multi-platform validation
  Platform_A_PD_L1 = platform_a_pdl1,
  Platform_B_PD_L1 = platform_b_pdl1,
  Platform_Agreement = platform_agreement,
  
  # Quality metrics
  Specimen_Adequacy = specimen_adequacy,
  Fixation_Time_Hours = fixation_time,
  Molecular_TAT_Days = molecular_tat_days,
  Diagnostic_Confidence = diagnostic_confidence,
  EQA_Score = eqa_score,
  
  # Clinical outcomes
  Overall_Survival_Months = survival_months,
  Death_Event = death_event,
  PFS_Months = pfs_months,
  Progression_Event = progression_event,
  Treatment_Response = treatment_response
)

# =============================================================================
# SAVE ENHANCED DATASETS
# =============================================================================

# Save main comprehensive dataset
save(ihc_molecular_comprehensive, file = "data/ihc_molecular_comprehensive.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(ihc_molecular_comprehensive, "data/ihc_molecular_comprehensive.omv")
  message("✓ Created ihc_molecular_comprehensive.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(ihc_molecular_comprehensive, "data/ihc_molecular_comprehensive.omv")
  message("✓ Created ihc_molecular_comprehensive.omv")
}
write.csv(ihc_molecular_comprehensive, "data/ihc_molecular_comprehensive.csv", row.names = FALSE)

# Create focused datasets for specific educational purposes

# 1. Biomarker validation study
biomarker_validation <- ihc_molecular_comprehensive %>%
  select(Patient_ID, Institution, Pathologist_Experience, Tumor_Type,
         PD_L1_TPS, Platform_A_PD_L1, Platform_B_PD_L1, Platform_Agreement,
         Ki67_Manual, Ki67_AI_Assisted, Ki67_Agreement,
         Specimen_Adequacy, Fixation_Time_Hours, EQA_Score) %>%
  filter(Tumor_Type %in% c("Lung_Adenocarcinoma", "Breast_Invasive_Ductal", "Melanoma"))

save(biomarker_validation, file = "data/biomarker_validation_study.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(biomarker_validation, "data/biomarker_validation_study.omv")
  message("✓ Created biomarker_validation_study.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(biomarker_validation, "data/biomarker_validation_study.omv")
  message("✓ Created biomarker_validation_study.omv")
}
write.csv(biomarker_validation, "data/biomarker_validation_study.csv", row.names = FALSE)

# 2. Precision oncology dataset
precision_oncology <- ihc_molecular_comprehensive %>%
  select(Patient_ID, Age, Gender, Tumor_Type, Grade,
         EGFR_Mutation, KRAS_Mutation, BRAF_Mutation, ALK_Fusion,
         HER2_IHC, HER2_FISH_Status, PD_L1_TPS, MSI_Status,
         Treatment_Response, PFS_Months, Progression_Event) %>%
  filter(Tumor_Type %in% c("Lung_Adenocarcinoma", "Breast_Invasive_Ductal", 
                           "Colorectal_Adenocarcinoma", "Melanoma"))

save(precision_oncology, file = "data/precision_oncology_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(precision_oncology, "data/precision_oncology_data.omv")
  message("✓ Created precision_oncology_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(precision_oncology, "data/precision_oncology_data.omv")
  message("✓ Created precision_oncology_data.omv")
}
write.csv(precision_oncology, "data/precision_oncology_data.csv", row.names = FALSE)

# 3. Digital pathology validation
digital_pathology_validation <- ihc_molecular_comprehensive %>%
  select(Patient_ID, Institution, Pathologist_Experience, Tumor_Type,
         Ki67_Manual, Ki67_AI_Assisted, Ki67_Agreement,
         CD8_Count_per_mm2, TIL_Percentage, p53_Pattern,
         Diagnostic_Confidence, Specimen_Adequacy) %>%
  filter(!is.na(Ki67_Manual))

save(digital_pathology_validation, file = "data/digital_pathology_validation.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(digital_pathology_validation, "data/digital_pathology_validation.omv")
  message("✓ Created digital_pathology_validation.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(digital_pathology_validation, "data/digital_pathology_validation.omv")
  message("✓ Created digital_pathology_validation.omv")
}
write.csv(digital_pathology_validation, "data/digital_pathology_validation.csv", row.names = FALSE)

# =============================================================================
# DATA SUMMARY AND VALIDATION
# =============================================================================

cat("=== Enhanced IHC & Molecular Pathology Dataset Generated ===\n")
cat("Total samples:", nrow(ihc_molecular_comprehensive), "\n")
cat("Total variables:", ncol(ihc_molecular_comprehensive), "\n\n")

cat("Institution distribution:\n")
print(table(ihc_molecular_comprehensive$Institution))

cat("\nTumor type distribution:\n")
print(table(ihc_molecular_comprehensive$Tumor_Type))

cat("\nMolecular alteration frequencies:\n")
cat("EGFR mutations:", round(mean(ihc_molecular_comprehensive$EGFR_Mutation == "Positive") * 100, 1), "%\n")
cat("KRAS mutations:", round(mean(ihc_molecular_comprehensive$KRAS_Mutation == "Positive") * 100, 1), "%\n")
cat("MSI-High cases:", round(mean(ihc_molecular_comprehensive$MSI_Status == "MSI-High") * 100, 1), "%\n")

cat("\nDigital pathology agreement rates:\n")
cat("Ki-67 manual vs AI agreement:", round(mean(ihc_molecular_comprehensive$Ki67_Agreement) * 100, 1), "%\n")
cat("Multi-platform PD-L1 agreement:", round(mean(ihc_molecular_comprehensive$Platform_Agreement) * 100, 1), "%\n")

cat("\nQuality metrics:\n")
cat("Adequate specimens:", round(mean(ihc_molecular_comprehensive$Specimen_Adequacy == "Adequate") * 100, 1), "%\n")
cat("Mean EQA score:", round(mean(ihc_molecular_comprehensive$EQA_Score), 1), "\n")

cat("\nDatasets created:\n")
cat("- ihc_molecular_comprehensive.rda/.csv (", nrow(ihc_molecular_comprehensive), " samples)\n")
cat("- biomarker_validation_study.rda/.csv (", nrow(biomarker_validation), " samples)\n")
cat("- precision_oncology_data.rda/.csv (", nrow(precision_oncology), " samples)\n")
cat("- digital_pathology_validation.rda/.csv (", nrow(digital_pathology_validation), " samples)\n")

cat("\n=== Enhanced molecular pathology data generation complete! ===\n")
cat("Datasets include modern biomarkers, digital pathology, and quality metrics\n")
cat("Educational focus: Multi-platform validation and precision oncology applications\n")
