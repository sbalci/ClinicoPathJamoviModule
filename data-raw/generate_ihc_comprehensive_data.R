# Comprehensive IHC Dataset Generation for ihcstats Testing
# This script generates synthetic data covering all four research methodologies:
# 1. Matsuoka 2011 - Ward's hierarchical clustering for colorectal cancer
# 2. Carvalho 2011 - Iterative marker selection for renal oncocytoma
# 3. Olsen 2006 - Differential diagnosis clustering for sarcomas
# 4. Sterlacci 2019 - TIL signature analysis for NSCLC

library(dplyr)
library(tibble)
library(magrittr)

set.seed(12345)  # For reproducible results

# Sample size for comprehensive testing
n_samples <- 300

# Generate Patient IDs
patient_id <- paste0("P", sprintf("%03d", 1:n_samples))

# =============================================================================
# TUMOR TYPE AND HISTOTYPE VARIABLES (for Olsen and Sterlacci methods)
# =============================================================================

# Primary tumor types for differential diagnosis (Olsen method)
tumor_types <- c(
  "Synovial_Sarcoma", "Leiomyosarcoma", "Rhabdomyosarcoma", 
  "Liposarcoma", "Fibrosarcoma", "Angiosarcoma",
  "Malignant_Peripheral_Nerve_Sheath_Tumor", "Undifferentiated_Sarcoma"
)

# NSCLC histotypes for Sterlacci TIL analysis
nsclc_histotypes <- c(
  "Adenocarcinoma", "Squamous_Cell_Carcinoma", 
  "Large_Cell_Carcinoma", "Neuroendocrine_Carcinoma"
)

# Assign tumor types and histotypes
tumor_type <- sample(tumor_types, n_samples, replace = TRUE, 
                    prob = c(0.15, 0.15, 0.12, 0.12, 0.12, 0.08, 0.08, 0.18))

histotype <- sample(nsclc_histotypes, n_samples, replace = TRUE,
                   prob = c(0.45, 0.35, 0.12, 0.08))

# =============================================================================
# IHC MARKERS - COMPREHENSIVE PANEL
# =============================================================================

# Core IHC markers for different tumor types (Olsen approach)
# Sarcoma markers
generate_ihc_marker <- function(n, positive_prob = 0.3, scoring_scale = "standard") {
  if (scoring_scale == "binary") {
    sample(c("Negative", "Positive"), n, replace = TRUE, 
           prob = c(1-positive_prob, positive_prob))
  } else if (scoring_scale == "carvalho") {
    sample(c("0", "1", "2"), n, replace = TRUE, 
           prob = c(0.4, 0.4, 0.2))
  } else if (scoring_scale == "standard") {
    sample(c("0", "1+", "2+", "3+"), n, replace = TRUE, 
           prob = c(0.3, 0.3, 0.25, 0.15))
  } else if (scoring_scale == "matsuoka") {
    sample(c("Mild", "Moderate", "Marked"), n, replace = TRUE,
           prob = c(0.4, 0.35, 0.25))
  }
}

# Sarcoma diagnostic markers (Olsen panel)
Vimentin <- generate_ihc_marker(n_samples, 0.85, "standard")  # Mesenchymal marker
SMA <- generate_ihc_marker(n_samples, 0.3, "standard")        # Smooth muscle
Desmin <- generate_ihc_marker(n_samples, 0.25, "standard")    # Muscle differentiation
S100 <- generate_ihc_marker(n_samples, 0.2, "standard")       # Neural differentiation
CD34 <- generate_ihc_marker(n_samples, 0.15, "standard")      # Endothelial/stem cell
Cytokeratin <- generate_ihc_marker(n_samples, 0.1, "standard") # Epithelial (usually negative)
EMA <- generate_ihc_marker(n_samples, 0.1, "standard")        # Epithelial membrane antigen
MDM2 <- generate_ihc_marker(n_samples, 0.2, "standard")       # Liposarcoma marker
CDK4 <- generate_ihc_marker(n_samples, 0.18, "standard")      # Liposarcoma marker
MyoD1 <- generate_ihc_marker(n_samples, 0.15, "standard")     # Rhabdomyosarcoma

# TIL and immune markers (Sterlacci panel)
CD3 <- generate_ihc_marker(n_samples, 0.6, "standard")        # Pan T-cell
CD4 <- generate_ihc_marker(n_samples, 0.4, "standard")        # Helper T-cells
CD8 <- generate_ihc_marker(n_samples, 0.5, "standard")        # Cytotoxic T-cells
CD20 <- generate_ihc_marker(n_samples, 0.3, "standard")       # B-cells
CD68 <- generate_ihc_marker(n_samples, 0.4, "standard")       # Macrophages
PD1 <- generate_ihc_marker(n_samples, 0.25, "standard")       # Immune checkpoint
PDL1 <- generate_ihc_marker(n_samples, 0.3, "standard")       # Immune checkpoint ligand
Granzyme_B <- generate_ihc_marker(n_samples, 0.35, "standard") # Cytotoxic activity
Perforin <- generate_ihc_marker(n_samples, 0.3, "standard")   # Cytotoxic activity
FOXP3 <- generate_ihc_marker(n_samples, 0.2, "standard")      # Regulatory T-cells

# Colorectal cancer markers (Matsuoka approach)
# Using Matsuoka 3-tier system for some markers
p53 <- generate_ihc_marker(n_samples, 0.4, "matsuoka")        # Tumor suppressor
Ki67 <- generate_ihc_marker(n_samples, 0.7, "matsuoka")       # Proliferation
VEGF <- generate_ihc_marker(n_samples, 0.5, "matsuoka")       # Angiogenesis
COX2 <- generate_ihc_marker(n_samples, 0.45, "matsuoka")      # Inflammation

# Renal markers (Carvalho approach - 0-2 scale)
CD117 <- generate_ihc_marker(n_samples, 0.6, "carvalho")      # c-Kit for oncocytoma
Vimentin_Carvalho <- generate_ihc_marker(n_samples, 0.4, "carvalho")
E_Cadherin <- generate_ihc_marker(n_samples, 0.5, "carvalho")
CK7 <- generate_ihc_marker(n_samples, 0.3, "carvalho")

# =============================================================================
# MULTI-REGION TUMOR ANALYSIS (Matsuoka method)
# =============================================================================

# Central region markers (typically different expression pattern)
p53_Central <- generate_ihc_marker(n_samples, 0.35, "matsuoka")
Ki67_Central <- generate_ihc_marker(n_samples, 0.6, "matsuoka")
VEGF_Central <- generate_ihc_marker(n_samples, 0.4, "matsuoka")
COX2_Central <- generate_ihc_marker(n_samples, 0.4, "matsuoka")

# Invasive front markers (usually higher expression)
p53_Invasive <- generate_ihc_marker(n_samples, 0.5, "matsuoka")
Ki67_Invasive <- generate_ihc_marker(n_samples, 0.8, "matsuoka")
VEGF_Invasive <- generate_ihc_marker(n_samples, 0.65, "matsuoka")
COX2_Invasive <- generate_ihc_marker(n_samples, 0.55, "matsuoka")

# =============================================================================
# SURVIVAL DATA (for Matsuoka prognostic clustering)
# =============================================================================

# Generate realistic survival data
# Overall survival time (months)
survival_time <- round(rexp(n_samples, rate = 1/24), 1)  # Mean ~24 months
survival_time <- pmax(survival_time, 0.5)  # Minimum 0.5 months
survival_time <- pmin(survival_time, 120)  # Maximum 120 months (10 years)

# Event indicator (1 = death, 0 = censored)
# Higher probability of events for advanced tumors
death_prob <- ifelse(
  tumor_type %in% c("Undifferentiated_Sarcoma", "Angiosarcoma"), 0.7,
  ifelse(tumor_type %in% c("Synovial_Sarcoma", "Leiomyosarcoma"), 0.5, 0.3)
)
survival_event <- rbinom(n_samples, 1, death_prob)

# Disease-free survival
dfs_time <- survival_time * runif(n_samples, 0.3, 0.9)
dfs_event <- rbinom(n_samples, 1, death_prob * 0.8)

# =============================================================================
# CLINICAL VARIABLES
# =============================================================================

# Patient demographics
age <- round(rnorm(n_samples, mean = 62, sd = 15))
age <- pmax(age, 18)  # Minimum age 18
age <- pmin(age, 95)  # Maximum age 95

gender <- sample(c("Male", "Female"), n_samples, replace = TRUE, prob = c(0.55, 0.45))

# Tumor characteristics
tumor_size <- round(rnorm(n_samples, mean = 5.2, sd = 2.8), 1)
tumor_size <- pmax(tumor_size, 0.5)  # Minimum 0.5 cm
tumor_size <- pmin(tumor_size, 25)   # Maximum 25 cm

tumor_grade <- sample(c("Grade_1", "Grade_2", "Grade_3"), n_samples, 
                     replace = TRUE, prob = c(0.2, 0.5, 0.3))

# TNM staging
T_stage <- sample(c("T1", "T2", "T3", "T4"), n_samples, 
                 replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
N_stage <- sample(c("N0", "N1", "N2"), n_samples, 
                 replace = TRUE, prob = c(0.6, 0.3, 0.1))
M_stage <- sample(c("M0", "M1"), n_samples, 
                 replace = TRUE, prob = c(0.8, 0.2))

# Treatment information
treatment <- sample(c("Surgery_Only", "Surgery_Chemo", "Surgery_Radio", 
                     "Surgery_Chemo_Radio", "Palliative"), n_samples,
                   replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1))

# =============================================================================
# CREATE COMPREHENSIVE DATASET
# =============================================================================

ihc_comprehensive_data <- tibble(
  # Patient identifiers
  Patient_ID = patient_id,
  
  # Demographics and clinical
  Age = age,
  Gender = as.factor(gender),
  Tumor_Type = as.factor(tumor_type),
  Histotype = as.factor(histotype),
  Tumor_Size_cm = tumor_size,
  Tumor_Grade = as.factor(tumor_grade),
  T_Stage = as.factor(T_stage),
  N_Stage = as.factor(N_stage),
  M_Stage = as.factor(M_stage),
  Treatment = as.factor(treatment),
  
  # Survival data
  Overall_Survival_Months = survival_time,
  Death_Event = survival_event,
  DFS_Months = dfs_time,
  DFS_Event = dfs_event,
  
  # Core IHC markers (standard 0-3+ scale)
  Vimentin = as.factor(Vimentin),
  SMA = as.factor(SMA),
  Desmin = as.factor(Desmin),
  S100 = as.factor(S100),
  CD34 = as.factor(CD34),
  Cytokeratin = as.factor(Cytokeratin),
  EMA = as.factor(EMA),
  MDM2 = as.factor(MDM2),
  CDK4 = as.factor(CDK4),
  MyoD1 = as.factor(MyoD1),
  
  # TIL and immune markers (Sterlacci approach)
  CD3 = as.factor(CD3),
  CD4 = as.factor(CD4),
  CD8 = as.factor(CD8),
  CD20 = as.factor(CD20),
  CD68 = as.factor(CD68),
  PD1 = as.factor(PD1),
  PDL1 = as.factor(PDL1),
  Granzyme_B = as.factor(Granzyme_B),
  Perforin = as.factor(Perforin),
  FOXP3 = as.factor(FOXP3),
  
  # Colorectal markers (Matsuoka 3-tier system)
  p53 = as.factor(p53),
  Ki67 = as.factor(Ki67),
  VEGF = as.factor(VEGF),
  COX2 = as.factor(COX2),
  
  # Renal markers (Carvalho 0-2 scale)
  CD117 = as.factor(CD117),
  Vimentin_Carvalho = as.factor(Vimentin_Carvalho),
  E_Cadherin = as.factor(E_Cadherin),
  CK7 = as.factor(CK7),
  
  # Multi-region analysis (Central region)
  p53_Central = as.factor(p53_Central),
  Ki67_Central = as.factor(Ki67_Central),
  VEGF_Central = as.factor(VEGF_Central),
  COX2_Central = as.factor(COX2_Central),
  
  # Multi-region analysis (Invasive front)
  p53_Invasive = as.factor(p53_Invasive),
  Ki67_Invasive = as.factor(Ki67_Invasive),
  VEGF_Invasive = as.factor(VEGF_Invasive),
  COX2_Invasive = as.factor(COX2_Invasive)
)

# =============================================================================
# ADD REALISTIC CORRELATIONS BETWEEN MARKERS
# =============================================================================

# Apply some realistic biological correlations
# CD4/CD8 ratio calculation for TIL analysis
cd4_numeric <- as.numeric(ihc_comprehensive_data$CD4)
cd8_numeric <- as.numeric(ihc_comprehensive_data$CD8)
cd4_cd8_ratio <- round(cd4_numeric / (cd8_numeric + 0.1), 2)

# TIL count simulation (for Sterlacci analysis)
cd8_til_count <- round(cd8_numeric * runif(n_samples, 10, 50))
granzyme_til_count <- round(as.numeric(ihc_comprehensive_data$Granzyme_B) * runif(n_samples, 5, 30))

# Add calculated variables
ihc_comprehensive_data <- ihc_comprehensive_data %>%
  mutate(
    CD4_CD8_Ratio = cd4_cd8_ratio,
    CD8_TIL_Count = cd8_til_count,
    Granzyme_TIL_Count = granzyme_til_count,
    
    # Create H-score like variables (0-300 scale) for some markers
    Ki67_HScore = round(as.numeric(Ki67) * runif(n_samples, 50, 100)),
    p53_HScore = round(as.numeric(p53) * runif(n_samples, 30, 90)),
    
    # Binary markers for some analyses
    CD34_Binary = ifelse(CD34 %in% c("2+", "3+"), "Positive", "Negative"),
    S100_Binary = ifelse(S100 %in% c("2+", "3+"), "Positive", "Negative"),
    
    # Risk categories for prognostic analysis
    Risk_Category = case_when(
      tumor_grade == "Grade_1" & M_stage == "M0" ~ "Low_Risk",
      tumor_grade == "Grade_2" & M_stage == "M0" ~ "Intermediate_Risk",
      tumor_grade == "Grade_3" | M_stage == "M1" ~ "High_Risk",
      TRUE ~ "Intermediate_Risk"
    ),
    
    # Immune signature classification (for Sterlacci analysis)
    Immune_Signature = case_when(
      CD8_TIL_Count > 25 & Granzyme_TIL_Count > 15 ~ "Hot",
      CD8_TIL_Count > 15 | Granzyme_TIL_Count > 10 ~ "Intermediate",
      TRUE ~ "Cold"
    )
  )

# Convert new variables to factors where appropriate
ihc_comprehensive_data$CD34_Binary <- as.factor(ihc_comprehensive_data$CD34_Binary)
ihc_comprehensive_data$S100_Binary <- as.factor(ihc_comprehensive_data$S100_Binary)
ihc_comprehensive_data$Risk_Category <- as.factor(ihc_comprehensive_data$Risk_Category)
ihc_comprehensive_data$Immune_Signature <- as.factor(ihc_comprehensive_data$Immune_Signature)

# =============================================================================
# SAVE DATASETS IN MULTIPLE FORMATS
# =============================================================================

# Save as R data file
save(ihc_comprehensive_data, file = "data/ihc_comprehensive_data.rda")

# Save as CSV for jamovi
write.csv(ihc_comprehensive_data, "data/ihc_comprehensive_data.csv", row.names = FALSE)

# Create smaller focused datasets for specific analyses

# 1. Sarcoma differential diagnosis dataset (Olsen approach)
sarcoma_data <- ihc_comprehensive_data %>%
  select(Patient_ID, Age, Gender, Tumor_Type, Tumor_Size_cm, Tumor_Grade,
         Vimentin, SMA, Desmin, S100, CD34, Cytokeratin, EMA, MDM2, CDK4, MyoD1,
         Overall_Survival_Months, Death_Event) %>%
  filter(Tumor_Type %in% tumor_types)

save(sarcoma_data, file = "data/sarcoma_ihc_data.rda")
write.csv(sarcoma_data, "data/sarcoma_ihc_data.csv", row.names = FALSE)

# 2. NSCLC TIL analysis dataset (Sterlacci approach)
nsclc_til_data <- ihc_comprehensive_data %>%
  select(Patient_ID, Age, Gender, Histotype, T_Stage, N_Stage, M_Stage,
         CD3, CD4, CD8, CD20, CD68, PD1, PDL1, Granzyme_B, Perforin, FOXP3,
         CD4_CD8_Ratio, CD8_TIL_Count, Granzyme_TIL_Count, Immune_Signature,
         Overall_Survival_Months, Death_Event) %>%
  filter(Histotype %in% nsclc_histotypes)

save(nsclc_til_data, file = "data/nsclc_til_data.rda")
write.csv(nsclc_til_data, "data/nsclc_til_data.csv", row.names = FALSE)

# 3. Colorectal prognostic dataset (Matsuoka approach)
colorectal_data <- ihc_comprehensive_data %>%
  select(Patient_ID, Age, Gender, Tumor_Grade, Risk_Category,
         p53, Ki67, VEGF, COX2, p53_HScore, Ki67_HScore,
         p53_Central, Ki67_Central, VEGF_Central, COX2_Central,
         p53_Invasive, Ki67_Invasive, VEGF_Invasive, COX2_Invasive,
         Overall_Survival_Months, Death_Event, DFS_Months, DFS_Event) %>%
  slice_head(n = 150)  # Smaller focused dataset

save(colorectal_data, file = "data/colorectal_ihc_data.rda")
write.csv(colorectal_data, "data/colorectal_ihc_data.csv", row.names = FALSE)

# 4. Renal oncocytoma dataset (Carvalho approach)
renal_data <- ihc_comprehensive_data %>%
  select(Patient_ID, Age, Gender, 
         CD117, Vimentin_Carvalho, E_Cadherin, CK7,
         Overall_Survival_Months, Death_Event) %>%
  slice_head(n = 100)  # Smaller focused dataset for iterative selection

save(renal_data, file = "data/renal_ihc_data.rda")
write.csv(renal_data, "data/renal_ihc_data.csv", row.names = FALSE)

# =============================================================================
# DATA SUMMARY AND VALIDATION
# =============================================================================

cat("=== IHC Comprehensive Dataset Generated Successfully ===\n")
cat("Total samples:", nrow(ihc_comprehensive_data), "\n")
cat("Total variables:", ncol(ihc_comprehensive_data), "\n\n")

cat("Tumor type distribution:\n")
print(table(ihc_comprehensive_data$Tumor_Type))

cat("\nHistotype distribution:\n")
print(table(ihc_comprehensive_data$Histotype))

cat("\nImmune signature distribution:\n")
print(table(ihc_comprehensive_data$Immune_Signature))

cat("\nRisk category distribution:\n")
print(table(ihc_comprehensive_data$Risk_Category))

cat("\nSurvival event rate:", 
    round(mean(ihc_comprehensive_data$Death_Event) * 100, 1), "%\n")

cat("\nDatasets saved:\n")
cat("- ihc_comprehensive_data.rda/.csv (", nrow(ihc_comprehensive_data), " samples)\n")
cat("- sarcoma_ihc_data.rda/.csv (", nrow(sarcoma_data), " samples)\n")
cat("- nsclc_til_data.rda/.csv (", nrow(nsclc_til_data), " samples)\n")
cat("- colorectal_ihc_data.rda/.csv (", nrow(colorectal_data), " samples)\n")
cat("- renal_ihc_data.rda/.csv (", nrow(renal_data), " samples)\n")

cat("\n=== Data generation complete! ===\n")