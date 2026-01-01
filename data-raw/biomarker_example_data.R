#' Example Datasets for Biomarker Response Association Analysis
#'
#' These datasets provide realistic examples for demonstrating biomarker-response relationships
#' across different clinical scenarios including precision oncology, pharmacogenomics, and clinical trials.
#'
#' @name biomarker_examples
#' @docType data
#' @format Data frames with various structures for different biomarker analysis scenarios
#' @author ClinicoPath package team

# Set seed for reproducible data generation
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(20241203)

# Helper functions for realistic biomarker data generation
generate_biomarker_expression <- function(n, low_mean = 2, high_mean = 8, low_prop = 0.6) {
  # Generate realistic biomarker expression levels (log scale)
  n_low <- round(n * low_prop)
  n_high <- n - n_low
  c(
    rlnorm(n_low, meanlog = low_mean, sdlog = 1.2),    # Low expressors
    rlnorm(n_high, meanlog = high_mean, sdlog = 0.8)   # High expressors
  )
}

generate_treatment_response <- function(biomarker_levels, threshold, response_prob_low = 0.2, response_prob_high = 0.8) {
  # Generate response based on biomarker levels
  high_expressors <- biomarker_levels >= threshold
  responses <- rep(NA, length(biomarker_levels))
  responses[!high_expressors] <- rbinom(sum(!high_expressors), 1, response_prob_low)
  responses[high_expressors] <- rbinom(sum(high_expressors), 1, response_prob_high)
  return(responses)
}

# 1. Predictive Biomarker Data - HER2+ Breast Cancer
#' @rdname biomarker_examples
#' @description \code{her2_breast_cancer_data}: Simulates HER2 expression and trastuzumab response
#' in breast cancer patients, demonstrating predictive biomarker analysis for targeted therapy.
her2_breast_cancer_data <- data.frame(
  # Patient identifiers
  patient_id = paste0("BC", sprintf("%04d", 1:500)),
  
  # Demographics
  age = round(rnorm(500, 58, 12)),
  menopausal_status = factor(sample(c("Premenopausal", "Postmenopausal"), 500, 
                                   replace = TRUE, prob = c(0.35, 0.65))),
  
  # Tumor characteristics
  tumor_size_cm = round(rlnorm(500, meanlog = 1.2, sdlog = 0.8), 1),
  grade = factor(sample(c("Grade 1", "Grade 2", "Grade 3"), 500, 
                       replace = TRUE, prob = c(0.15, 0.45, 0.40))),
  er_status = factor(sample(c("Positive", "Negative"), 500, 
                           replace = TRUE, prob = c(0.75, 0.25))),
  pr_status = factor(sample(c("Positive", "Negative"), 500, 
                           replace = TRUE, prob = c(0.65, 0.35))),
  
  # HER2 expression levels (key biomarker)
  her2_expression = c(
    # HER2 negative patients (n=400)
    rlnorm(400, meanlog = 1.5, sdlog = 0.8),  # Low expression
    # HER2 positive patients (n=100) 
    rlnorm(100, meanlog = 3.2, sdlog = 0.6)   # High expression
  ),
  
  # Treatment assignment
  treatment_arm = factor(sample(c("Trastuzumab + Chemotherapy", "Chemotherapy Alone"), 500,
                               replace = TRUE, prob = c(0.6, 0.4))),
  
  # Treatment response (pathological complete response)
  pcr_response = NA,
  
  # Progression-free survival (months)
  pfs_months = NA,
  
  # Overall survival status
  os_status = factor(sample(c("Alive", "Deceased"), 500, replace = TRUE, prob = c(0.8, 0.2))),
  
  # Follow-up time
  followup_months = round(runif(500, 12, 60))
)

# Generate responses based on HER2 status and treatment
her2_positive <- her2_breast_cancer_data$her2_expression >= 10
trastuzumab_treated <- her2_breast_cancer_data$treatment_arm == "Trastuzumab + Chemotherapy"

# pCR response rates: HER2+ with trastuzumab (65%), HER2+ without (25%), HER2- (15%)
her2_breast_cancer_data$pcr_response[her2_positive & trastuzumab_treated] <- 
  rbinom(sum(her2_positive & trastuzumab_treated), 1, 0.65)
her2_breast_cancer_data$pcr_response[her2_positive & !trastuzumab_treated] <- 
  rbinom(sum(her2_positive & !trastuzumab_treated), 1, 0.25)
her2_breast_cancer_data$pcr_response[!her2_positive] <- 
  rbinom(sum(!her2_positive), 1, 0.15)

her2_breast_cancer_data$pcr_response <- factor(her2_breast_cancer_data$pcr_response, 
                                              levels = c(0, 1), labels = c("No pCR", "pCR"))

# Generate PFS based on HER2 status and response
base_pfs <- ifelse(her2_positive & trastuzumab_treated, 24, 
                  ifelse(her2_positive, 18, 15))
her2_breast_cancer_data$pfs_months <- round(rexp(500, rate = 1/base_pfs) + 
                                           rnorm(500, 0, 3))
her2_breast_cancer_data$pfs_months[her2_breast_cancer_data$pfs_months < 1] <- 1

# 2. PD-L1 Immunotherapy Data
#' @rdname biomarker_examples  
#' @description \code{pdl1_immunotherapy_data}: Simulates PD-L1 expression and immunotherapy response
#' across different cancer types, demonstrating biomarker validation across indications.
pdl1_immunotherapy_data <- data.frame(
  # Patient identifiers  
  patient_id = paste0("IO", sprintf("%04d", 1:400)),
  
  # Cancer characteristics
  cancer_type = factor(sample(c("NSCLC", "Melanoma", "Renal Cell", "Bladder", "Head & Neck"), 400,
                             replace = TRUE, prob = c(0.4, 0.25, 0.15, 0.1, 0.1))),
  stage = factor(sample(c("Stage III", "Stage IV"), 400, replace = TRUE, prob = c(0.3, 0.7))),
  
  # Patient characteristics
  age = round(rnorm(400, 65, 10)),
  sex = factor(sample(c("Male", "Female"), 400, replace = TRUE, prob = c(0.6, 0.4))),
  ecog_ps = factor(sample(c("0", "1", "2"), 400, replace = TRUE, prob = c(0.4, 0.5, 0.1))),
  
  # Biomarker measurements
  pdl1_percentage = c(
    # PD-L1 negative (<1%)
    runif(120, 0, 0.9),
    # PD-L1 low (1-49%)  
    runif(160, 1, 49),
    # PD-L1 high (≥50%)
    runif(120, 50, 100)
  ),
  
  # Tumor mutational burden
  tmb_mutations_mb = c(
    rlnorm(200, meanlog = 1.8, sdlog = 1.0),  # TMB-low
    rlnorm(200, meanlog = 3.2, sdlog = 0.8)   # TMB-high
  ),
  
  # Microsatellite instability
  msi_status = factor(sample(c("MSS", "MSI-High"), 400, replace = TRUE, prob = c(0.85, 0.15))),
  
  # Treatment details
  immunotherapy_type = factor(sample(c("Pembrolizumab", "Nivolumab", "Atezolizumab", "Combination"), 400,
                                    replace = TRUE)),
  
  # Response outcomes
  best_response = NA,
  response_duration_months = NA,
  progression_free_survival = NA,
  
  # Immune-related adverse events
  ir_adverse_events = factor(sample(c("None", "Grade 1-2", "Grade 3-4"), 400,
                                   replace = TRUE, prob = c(0.6, 0.3, 0.1)))
)

# Generate responses based on PD-L1 expression
pdl1_high <- pdl1_immunotherapy_data$pdl1_percentage >= 50
pdl1_low <- pdl1_immunotherapy_data$pdl1_percentage >= 1 & pdl1_immunotherapy_data$pdl1_percentage < 50
pdl1_negative <- pdl1_immunotherapy_data$pdl1_percentage < 1

# Response rates: PD-L1 high (45%), low (25%), negative (10%)
responses <- rep(NA, 400)
responses[pdl1_high] <- sample(c("CR", "PR", "SD", "PD"), sum(pdl1_high), 
                              replace = TRUE, prob = c(0.05, 0.40, 0.35, 0.20))
responses[pdl1_low] <- sample(c("CR", "PR", "SD", "PD"), sum(pdl1_low),
                             replace = TRUE, prob = c(0.02, 0.23, 0.40, 0.35))
responses[pdl1_negative] <- sample(c("CR", "PR", "SD", "PD"), sum(pdl1_negative),
                                  replace = TRUE, prob = c(0.01, 0.09, 0.30, 0.60))

pdl1_immunotherapy_data$best_response <- factor(responses, levels = c("CR", "PR", "SD", "PD"))

# Generate survival outcomes
base_pfs <- ifelse(pdl1_high, 8.5, ifelse(pdl1_low, 4.2, 2.8))
pdl1_immunotherapy_data$progression_free_survival <- round(rexp(400, rate = 1/base_pfs) + 
                                                          rnorm(400, 0, 1))
pdl1_immunotherapy_data$progression_free_survival[pdl1_immunotherapy_data$progression_free_survival < 1] <- 1

# 3. Pharmacogenomic Data - CYP2D6 and Drug Metabolism
#' @rdname biomarker_examples
#' @description \code{cyp2d6_pharmacogenomics_data}: Simulates CYP2D6 genotype and drug metabolism
#' demonstrating pharmacogenomic biomarker analysis for personalized dosing.
cyp2d6_pharmacogenomics_data <- data.frame(
  # Patient identifiers
  patient_id = paste0("PGx", sprintf("%04d", 1:300)),
  
  # Demographics
  age = round(rnorm(300, 55, 15)),
  sex = factor(sample(c("Male", "Female"), 300, replace = TRUE)),
  ethnicity = factor(sample(c("Caucasian", "African American", "Hispanic", "Asian", "Other"), 300,
                           replace = TRUE, prob = c(0.65, 0.15, 0.12, 0.06, 0.02))),
  
  # Body characteristics
  weight_kg = round(rnorm(300, 75, 15)),
  bmi = round(rnorm(300, 26, 4), 1),
  
  # CYP2D6 genotype and phenotype
  cyp2d6_genotype = factor(sample(c("*1/*1", "*1/*2", "*2/*2", "*1/*4", "*4/*4", 
                                   "*1/*3", "*3/*4", "*1xN/*1", "*2xN/*2"), 300,
                                 replace = TRUE, prob = c(0.25, 0.20, 0.10, 0.15, 0.05, 
                                                         0.08, 0.02, 0.10, 0.05))),
  
  cyp2d6_phenotype = factor(c(
    # Poor metabolizers (7%)
    rep("Poor Metabolizer", 21),
    # Intermediate metabolizers (20%)  
    rep("Intermediate Metabolizer", 60),
    # Normal/Extensive metabolizers (65%)
    rep("Extensive Metabolizer", 195),
    # Ultrarapid metabolizers (8%)
    rep("Ultrarapid Metabolizer", 24)
  )),
  
  # Drug characteristics
  drug_name = factor(sample(c("Codeine", "Tramadol", "Venlafaxine", "Risperidone", "Tamoxifen"), 300,
                           replace = TRUE)),
  prescribed_dose_mg = round(rnorm(300, 50, 20)),
  
  # Pharmacokinetic outcomes
  drug_clearance_ml_min = NA,
  plasma_concentration_ng_ml = NA,
  metabolite_ratio = NA,
  
  # Clinical outcomes  
  therapeutic_response = factor(sample(c("Excellent", "Good", "Fair", "Poor"), 300,
                                      replace = TRUE)),
  adverse_events = factor(sample(c("None", "Mild", "Moderate", "Severe"), 300,
                                replace = TRUE, prob = c(0.6, 0.25, 0.12, 0.03))),
  
  # Monitoring data
  days_to_steady_state = round(rnorm(300, 5, 2)),
  dose_adjustments = factor(sample(c("None", "Increase", "Decrease"), 300,
                                  replace = TRUE, prob = c(0.7, 0.15, 0.15)))
)

# Generate pharmacokinetic parameters based on CYP2D6 phenotype
phenotype_effects <- data.frame(
  phenotype = c("Poor Metabolizer", "Intermediate Metabolizer", 
                "Extensive Metabolizer", "Ultrarapid Metabolizer"),
  clearance_multiplier = c(0.2, 0.6, 1.0, 2.5),
  concentration_multiplier = c(5.0, 2.0, 1.0, 0.4)
)

for (i in 1:nrow(cyp2d6_pharmacogenomics_data)) {
  phenotype <- cyp2d6_pharmacogenomics_data$cyp2d6_phenotype[i]
  multiplier_row <- phenotype_effects[phenotype_effects$phenotype == phenotype, ]
  
  # Drug clearance
  base_clearance <- rnorm(1, 100, 25)
  cyp2d6_pharmacogenomics_data$drug_clearance_ml_min[i] <- 
    round(base_clearance * multiplier_row$clearance_multiplier)
  
  # Plasma concentration  
  base_concentration <- rnorm(1, 20, 5)
  cyp2d6_pharmacogenomics_data$plasma_concentration_ng_ml[i] <- 
    round(base_concentration * multiplier_row$concentration_multiplier, 1)
  
  # Metabolite ratio
  cyp2d6_pharmacogenomics_data$metabolite_ratio[i] <- 
    round(runif(1, 0.1, 2.0) * multiplier_row$clearance_multiplier, 2)
}

# 4. Multi-Biomarker Panel Data - Oncotype DX
#' @rdname biomarker_examples
#' @description \code{oncotype_dx_data}: Simulates multi-gene expression panel for breast cancer
#' recurrence prediction, demonstrating composite biomarker analysis.
oncotype_dx_data <- data.frame(
  # Patient identifiers
  patient_id = paste0("ODX", sprintf("%04d", 1:250)),
  
  # Clinical characteristics
  age_at_diagnosis = round(rnorm(250, 58, 12)),
  tumor_size_cm = round(rlnorm(250, meanlog = 1.0, sdlog = 0.6), 1),
  nodes_positive = round(rpois(250, lambda = 0.8)),
  grade = factor(sample(c("Grade 1", "Grade 2", "Grade 3"), 250, 
                       replace = TRUE, prob = c(0.2, 0.6, 0.2))),
  
  # Hormone receptor status
  er_status = factor(rep("Positive", 250)),  # All ER+ for Oncotype DX
  pr_status = factor(sample(c("Positive", "Negative"), 250, replace = TRUE, prob = c(0.8, 0.2))),
  her2_status = factor(rep("Negative", 250)), # All HER2- for Oncotype DX
  
  # Individual gene expression levels (normalized scores)
  ki67_expression = round(rnorm(250, 50, 20)),
  survivin_expression = round(rnorm(250, 45, 18)),
  cyclin_b1_expression = round(rnorm(250, 48, 22)),
  mybl2_expression = round(rnorm(250, 52, 19)),
  mmp11_expression = round(rnorm(250, 47, 21)),
  ctsl2_expression = round(rnorm(250, 49, 17)),
  gstm1_expression = round(rnorm(250, 46, 20)),
  cd68_expression = round(rnorm(250, 51, 18)),
  bag1_expression = round(rnorm(250, 53, 16)),
  
  # Estrogen receptor genes
  esr1_expression = round(rnorm(250, 75, 15)),
  pgr_expression = round(rnorm(250, 70, 18)),
  bcl2_expression = round(rnorm(250, 72, 16)),
  scube2_expression = round(rnorm(250, 74, 14)),
  
  # HER2 group genes
  grb7_expression = round(rnorm(250, 25, 8)),
  erbb2_expression = round(rnorm(250, 23, 7)),
  
  # Reference genes  
  actb_expression = round(rnorm(250, 100, 5)),
  gapdh_expression = round(rnorm(250, 98, 4)),
  rplp0_expression = round(rnorm(250, 99, 6)),
  gus_expression = round(rnorm(250, 101, 5)),
  tfrc_expression = round(rnorm(250, 97, 7)),
  
  # Calculated scores
  recurrence_score = NA,
  risk_category = NA,
  
  # Clinical outcomes
  chemotherapy_given = factor(sample(c("Yes", "No"), 250, replace = TRUE, prob = c(0.3, 0.7))),
  recurrence_status = factor(sample(c("No Recurrence", "Recurrence"), 250, 
                                   replace = TRUE, prob = c(0.85, 0.15))),
  time_to_recurrence_years = round(runif(250, 1, 10), 1),
  overall_survival_years = round(runif(250, 2, 15), 1)
)

# Calculate Oncotype DX Recurrence Score (simplified algorithm)
# Actual algorithm is proprietary, this is a simplified version
proliferation_score <- (oncotype_dx_data$ki67_expression + 
                       oncotype_dx_data$survivin_expression + 
                       oncotype_dx_data$cyclin_b1_expression + 
                       oncotype_dx_data$mybl2_expression) / 4

invasion_score <- (oncotype_dx_data$mmp11_expression + 
                  oncotype_dx_data$ctsl2_expression) / 2

er_score <- (oncotype_dx_data$esr1_expression + 
            oncotype_dx_data$pgr_expression + 
            oncotype_dx_data$bcl2_expression + 
            oncotype_dx_data$scube2_expression) / 4

her2_score <- (oncotype_dx_data$grb7_expression + 
              oncotype_dx_data$erbb2_expression) / 2

# Simplified recurrence score calculation
oncotype_dx_data$recurrence_score <- round(
  (0.47 * proliferation_score - 0.34 * er_score + 
   0.10 * invasion_score + 0.05 * her2_score) + rnorm(250, 0, 5)
)

# Ensure score is in 0-100 range
oncotype_dx_data$recurrence_score[oncotype_dx_data$recurrence_score < 0] <- 0
oncotype_dx_data$recurrence_score[oncotype_dx_data$recurrence_score > 100] <- 100

# Risk categorization
oncotype_dx_data$risk_category <- factor(
  ifelse(oncotype_dx_data$recurrence_score < 18, "Low Risk",
         ifelse(oncotype_dx_data$recurrence_score <= 30, "Intermediate Risk", "High Risk")),
  levels = c("Low Risk", "Intermediate Risk", "High Risk")
)

# 5. Longitudinal Biomarker Data - PSA Monitoring
#' @rdname biomarker_examples
#' @description \code{psa_longitudinal_data}: Simulates PSA monitoring in prostate cancer patients
#' demonstrating longitudinal biomarker analysis and treatment response monitoring.
psa_longitudinal_data <- data.frame(
  # Patient and visit identifiers
  patient_id = rep(paste0("PSA", sprintf("%03d", 1:100)), each = 8),
  visit_number = rep(1:8, 100),
  months_from_baseline = rep(c(0, 3, 6, 9, 12, 18, 24, 36), 100),
  
  # Patient characteristics (repeated for each visit)
  age_at_baseline = rep(round(rnorm(100, 68, 8)), each = 8),
  gleason_score = rep(factor(sample(c("6", "7", "8", "9", "10"), 100, 
                                   replace = TRUE, prob = c(0.1, 0.4, 0.3, 0.15, 0.05))), each = 8),
  stage_at_diagnosis = rep(factor(sample(c("T1", "T2", "T3", "T4"), 100,
                                        replace = TRUE, prob = c(0.3, 0.4, 0.25, 0.05))), each = 8),
  
  # Treatment information
  treatment_type = rep(factor(sample(c("Active Surveillance", "Radical Prostatectomy", 
                                      "Radiation Therapy", "Hormone Therapy", "Chemotherapy"), 100,
                                    replace = TRUE, prob = c(0.2, 0.3, 0.25, 0.15, 0.1))), each = 8),
  
  # PSA measurements
  psa_ng_ml = NA,
  psa_doubling_time_months = NA,
  psa_nadir_achieved = rep(factor(sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.7, 0.3))), each = 8),
  
  # Clinical assessments
  clinical_response = rep(factor(sample(c("Complete Response", "Partial Response", 
                                         "Stable Disease", "Progressive Disease"), 100,
                                       replace = TRUE, prob = c(0.15, 0.35, 0.35, 0.15))), each = 8),
  
  # Quality of life measures
  pain_score = round(runif(800, 0, 10)),
  performance_status = factor(sample(c("0", "1", "2", "3"), 800, 
                                    replace = TRUE, prob = c(0.4, 0.4, 0.15, 0.05)))
)

# Generate realistic PSA trajectories based on treatment and response
for (i in 1:100) {
  patient_indices <- ((i-1)*8 + 1):(i*8)
  treatment <- as.character(psa_longitudinal_data$treatment_type[patient_indices[1]])
  response <- as.character(psa_longitudinal_data$clinical_response[patient_indices[1]])
  
  # Baseline PSA
  baseline_psa <- rlnorm(1, meanlog = 2.5, sdlog = 1.2)  # ~12 ng/mL median
  
  # Treatment effect patterns
  if (treatment == "Active Surveillance") {
    # Slow increase over time
    psa_trajectory <- baseline_psa * (1 + 0.1 * c(0, 3, 6, 9, 12, 18, 24, 36)/12)
  } else if (treatment == "Radical Prostatectomy") {
    # Dramatic drop to undetectable levels
    psa_trajectory <- c(baseline_psa, rep(0.1, 7)) * exp(rnorm(8, 0, 0.2))
  } else if (treatment == "Radiation Therapy") {
    # Gradual decline then stabilization  
    decline_factor <- exp(-c(0, 3, 6, 9, 12, 18, 24, 36)/12)
    psa_trajectory <- baseline_psa * decline_factor * (0.3 + 0.7 * decline_factor)
  } else if (treatment == "Hormone Therapy") {
    # Rapid initial decline, then potential rise
    if (response %in% c("Complete Response", "Partial Response")) {
      psa_trajectory <- baseline_psa * c(1, 0.3, 0.15, 0.1, 0.12, 0.18, 0.25, 0.4)
    } else {
      psa_trajectory <- baseline_psa * c(1, 0.5, 0.4, 0.5, 0.8, 1.2, 1.8, 2.5)
    }
  } else { # Chemotherapy
    # Variable response
    if (response == "Complete Response") {
      psa_trajectory <- baseline_psa * c(1, 0.6, 0.3, 0.2, 0.15, 0.1, 0.1, 0.15)
    } else if (response == "Partial Response") {
      psa_trajectory <- baseline_psa * c(1, 0.7, 0.5, 0.4, 0.4, 0.5, 0.6, 0.8)
    } else {
      psa_trajectory <- baseline_psa * c(1, 1.1, 1.3, 1.6, 2.0, 2.5, 3.2, 4.0)
    }
  }
  
  # Add measurement noise
  psa_trajectory <- psa_trajectory * exp(rnorm(8, 0, 0.15))
  psa_trajectory[psa_trajectory < 0.01] <- 0.01  # Lower limit of detection
  
  psa_longitudinal_data$psa_ng_ml[patient_indices] <- round(psa_trajectory, 2)
  
  # Calculate PSA doubling time (simplified)
  if (length(psa_trajectory) >= 3) {
    psa_dt <- numeric(8)
    for (j in 3:8) {
      if (psa_trajectory[j] > psa_trajectory[j-2] && psa_trajectory[j-2] > 0) {
        months_interval <- psa_longitudinal_data$months_from_baseline[patient_indices[j]] - 
                          psa_longitudinal_data$months_from_baseline[patient_indices[j-2]]
        fold_change <- psa_trajectory[j] / psa_trajectory[j-2]
        psa_dt[j] <- months_interval * log(2) / log(fold_change)
      }
    }
    psa_longitudinal_data$psa_doubling_time_months[patient_indices] <- round(psa_dt, 1)
  }
}

# Export all datasets
use_data_multi_format(her2_breast_cancer_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(pdl1_immunotherapy_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(cyp2d6_pharmacogenomics_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(oncotype_dx_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(psa_longitudinal_data, overwrite = TRUE, save_csv = TRUE)

# Generate comprehensive summary
cat("✅ Created 5 comprehensive datasets for biomarker response analysis:\n\n")

cat("🎗️ her2_breast_cancer_data (n=500):\n")
cat("   - HER2 expression levels and trastuzumab response\n")
cat("   - Pathological complete response outcomes\n")
cat("   - Demonstrates predictive biomarker analysis for targeted therapy\n")
cat("   - Real-world breast cancer treatment scenarios\n\n")

cat("🔬 pdl1_immunotherapy_data (n=400):\n")
cat("   - PD-L1 expression across multiple cancer types\n")
cat("   - Immunotherapy response patterns (CR/PR/SD/PD)\n")
cat("   - TMB and MSI status as companion biomarkers\n")
cat("   - Multi-cancer immunotherapy validation scenarios\n\n")

cat("💊 cyp2d6_pharmacogenomics_data (n=300):\n")
cat("   - CYP2D6 genotype and phenotype relationships\n")
cat("   - Drug metabolism and pharmacokinetic outcomes\n")
cat("   - Personalized dosing scenarios\n")
cat("   - Demonstrates pharmacogenomic biomarker utility\n\n")

cat("🧬 oncotype_dx_data (n=250):\n")
cat("   - Multi-gene expression panel (21-gene signature)\n")
cat("   - Composite recurrence score calculation\n")
cat("   - Risk stratification for treatment decisions\n")
cat("   - Demonstrates complex biomarker panel analysis\n\n")

cat("📈 psa_longitudinal_data (n=800 observations, 100 patients):\n")
cat("   - PSA monitoring over 36 months\n")
cat("   - Treatment response trajectories\n")
cat("   - PSA doubling time calculations\n")
cat("   - Demonstrates longitudinal biomarker analysis\n\n")

cat("💡 Usage examples:\n")
cat("   biomarkerresponse(data = her2_breast_cancer_data, biomarker = 'her2_expression', response = 'pcr_response')\n")
cat("   biomarkerresponse(data = pdl1_immunotherapy_data, biomarker = 'pdl1_percentage', response = 'best_response')\n")
cat("   biomarkerresponse(data = cyp2d6_pharmacogenomics_data, biomarker = 'drug_clearance_ml_min', response = 'cyp2d6_phenotype')\n")
cat("   biomarkerresponse(data = oncotype_dx_data, biomarker = 'recurrence_score', response = 'risk_category')\n")
cat("   biomarkerresponse(data = psa_longitudinal_data, biomarker = 'psa_ng_ml', response = 'clinical_response')\n\n")

cat("🎯 Key features:\n")
cat("   - Clinically realistic biomarker distributions\n")
cat("   - Multiple response types (binary, categorical, continuous)\n")
cat("   - Diverse therapeutic areas and biomarker classes\n")
cat("   - Sufficient sample sizes for robust analysis\n")
cat("   - Rich metadata for advanced analysis and interpretation\n")
cat("   - Longitudinal data for time-course analysis\n")
