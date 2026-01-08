# ═══════════════════════════════════════════════════════════
# Test Data Generation: sequentialtests
# ═══════════════════════════════════════════════════════════
# Function: Sequential Testing Analysis
# Purpose: Reference datasets for sequential diagnostic testing strategies
# Note: This is a calculator function - datasets show realistic test parameters
# ═══════════════════════════════════════════════════════════

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# ═══════════════════════════════════════════════════════════
# 1. sequentialtests_reference: Common Test Combinations
# ═══════════════════════════════════════════════════════════
sequentialtests_reference <- tibble(
  scenario = c(
    "COVID-19 Screening",
    "Breast Cancer Screening",
    "MI Emergency",
    "TB Screening",
    "Prostate Cancer",
    "HIV Screening",
    "Stroke Emergency",
    "Colorectal Cancer",
    "Pregnancy Test",
    "Strep Throat"
  ),
  test1_name = c(
    "Rapid Antigen",
    "Mammography",
    "Troponin",
    "Chest X-ray",
    "PSA",
    "ELISA",
    "Clinical Assessment",
    "FOBT",
    "Home Pregnancy Test",
    "Rapid Strep"
  ),
  test1_sens = c(0.85, 0.87, 0.95, 0.70, 0.90, 0.995, 0.85, 0.75, 0.99, 0.86),
  test1_spec = c(0.95, 0.88, 0.85, 0.92, 0.70, 0.995, 0.75, 0.95, 0.98, 0.95),
  test1_cost = c(5, 100, 50, 30, 40, 30, 0, 10, 10, 15),
  test2_name = c(
    "RT-PCR",
    "Biopsy",
    "ECG",
    "Sputum Culture",
    "MRI",
    "Western Blot",
    "CT Scan",
    "Colonoscopy",
    "Blood hCG",
    "Throat Culture"
  ),
  test2_sens = c(0.95, 0.95, 0.90, 0.85, 0.85, 0.999, 0.92, 0.95, 0.999, 0.99),
  test2_spec = c(0.99, 0.98, 0.92, 0.98, 0.88, 0.999, 0.95, 0.99, 0.999, 0.99),
  test2_cost = c(100, 500, 100, 80, 1200, 200, 800, 1500, 50, 40),
  prevalence = c(0.05, 0.006, 0.15, 0.02, 0.25, 0.001, 0.10, 0.05, 0.50, 0.30),
  optimal_strategy = c(
    "serial_positive",
    "serial_positive",
    "parallel",
    "serial_positive",
    "serial_negative",
    "serial_positive",
    "parallel",
    "serial_positive",
    "serial_positive",
    "serial_positive"
  ),
  clinical_setting = c(
    "Community screening",
    "Population screening",
    "Emergency department",
    "High-risk screening",
    "Screening + exclusion",
    "Blood donor screening",
    "Emergency department",
    "Average-risk screening",
    "Home testing",
    "Outpatient clinic"
  )
)

# ═══════════════════════════════════════════════════════════
# 2. sequentialtests_covid: Detailed COVID-19 Testing Scenarios
# ═══════════════════════════════════════════════════════════
sequentialtests_covid <- tibble(
  scenario = rep(c("Community", "Hospital", "Outbreak", "Travel"), each = 3),
  strategy = rep(c("serial_positive", "serial_negative", "parallel"), 4),
  test1_name = "Rapid Antigen",
  test1_sens = 0.85,
  test1_spec = 0.95,
  test1_cost = 5,
  test2_name = "RT-PCR",
  test2_sens = 0.95,
  test2_spec = 0.99,
  test2_cost = 100,
  prevalence = c(
    rep(0.02, 3),  # Community
    rep(0.15, 3),  # Hospital
    rep(0.40, 3),  # Outbreak
    rep(0.05, 3)   # Travel
  ),
  population_size = 10000
)

# ═══════════════════════════════════════════════════════════
# 3. sequentialtests_cancer: Cancer Screening Scenarios
# ═══════════════════════════════════════════════════════════
sequentialtests_cancer <- tibble(
  cancer_type = c(
    "Breast",
    "Colorectal",
    "Prostate",
    "Lung",
    "Cervical",
    "Ovarian"
  ),
  test1_name = c(
    "Mammography",
    "FOBT",
    "PSA",
    "Low-dose CT",
    "Pap Smear",
    "CA-125"
  ),
  test1_sens = c(0.87, 0.75, 0.90, 0.94, 0.85, 0.80),
  test1_spec = c(0.88, 0.95, 0.70, 0.73, 0.95, 0.75),
  test1_cost = c(100, 10, 40, 250, 50, 40),
  test2_name = c(
    "Biopsy",
    "Colonoscopy",
    "MRI + Biopsy",
    "Biopsy",
    "Colposcopy",
    "Ultrasound + Biopsy"
  ),
  test2_sens = c(0.95, 0.95, 0.85, 0.98, 0.92, 0.90),
  test2_spec = c(0.98, 0.99, 0.88, 0.99, 0.98, 0.95),
  test2_cost = c(500, 1500, 1200, 800, 300, 600),
  prevalence = c(0.006, 0.05, 0.25, 0.015, 0.008, 0.002),
  age_group = c("50-69", "50+", "50+", "55-80", "21-65", "50-70"),
  strategy = "serial_positive"
)

# ═══════════════════════════════════════════════════════════
# 4. sequentialtests_emergency: Emergency Department Scenarios
# ═══════════════════════════════════════════════════════════
sequentialtests_emergency <- tibble(
  condition = c(
    "Myocardial Infarction",
    "Stroke",
    "Pulmonary Embolism",
    "Appendicitis",
    "Meningitis",
    "Sepsis"
  ),
  test1_name = c(
    "Troponin",
    "NIHSS",
    "D-dimer",
    "Alvarado Score",
    "Clinical Exam",
    "qSOFA"
  ),
  test1_sens = c(0.95, 0.85, 0.95, 0.80, 0.70, 0.85),
  test1_spec = c(0.85, 0.75, 0.40, 0.75, 0.60, 0.70),
  test1_cost = c(50, 0, 30, 0, 0, 0),
  test2_name = c(
    "ECG",
    "CT Brain",
    "CT Angiography",
    "CT Abdomen",
    "LP + Culture",
    "Blood Culture"
  ),
  test2_sens = c(0.90, 0.92, 0.90, 0.95, 0.95, 0.80),
  test2_spec = c(0.92, 0.95, 0.95, 0.98, 0.98, 0.95),
  test2_cost = c(100, 800, 600, 500, 200, 100),
  prevalence = c(0.15, 0.10, 0.05, 0.20, 0.02, 0.12),
  strategy = "parallel",
  urgency = "High"
)

# ═══════════════════════════════════════════════════════════
# 5. sequentialtests_infectious: Infectious Disease Testing
# ═══════════════════════════════════════════════════════════
sequentialtests_infectious <- tibble(
  disease = c(
    "HIV",
    "Hepatitis C",
    "Tuberculosis",
    "Syphilis",
    "Influenza",
    "Malaria"
  ),
  test1_name = c(
    "ELISA",
    "Anti-HCV",
    "Chest X-ray",
    "RPR",
    "Rapid Influenza",
    "Rapid Diagnostic"
  ),
  test1_sens = c(0.995, 0.97, 0.70, 0.78, 0.85, 0.95),
  test1_spec = c(0.995, 0.99, 0.92, 0.85, 0.90, 0.90),
  test1_cost = c(30, 40, 30, 15, 20, 5),
  test2_name = c(
    "Western Blot",
    "HCV RNA",
    "Sputum Culture",
    "TPPA",
    "RT-PCR",
    "Microscopy"
  ),
  test2_sens = c(0.999, 0.99, 0.85, 0.95, 0.95, 0.90),
  test2_spec = c(0.999, 0.999, 0.98, 0.98, 0.98, 0.98),
  test2_cost = c(200, 150, 80, 50, 100, 30),
  prevalence = c(0.001, 0.015, 0.02, 0.005, 0.10, 0.30),
  strategy = "serial_positive",
  population = c("Blood donors", "High risk", "High risk", "STI clinic", "Symptomatic", "Endemic area")
)

# ═══════════════════════════════════════════════════════════
# 6. sequentialtests_cost_comparison: Cost-Effectiveness Examples
# ═══════════════════════════════════════════════════════════
sequentialtests_cost_comparison <- tibble(
  scenario = rep(c("Low prevalence", "Medium prevalence", "High prevalence"), each = 3),
  strategy = rep(c("serial_positive", "serial_negative", "parallel"), 3),
  test1_name = "Screening Test",
  test1_sens = 0.90,
  test1_spec = 0.85,
  test1_cost = 50,
  test2_name = "Confirmatory Test",
  test2_sens = 0.85,
  test2_spec = 0.95,
  test2_cost = 300,
  prevalence = c(
    rep(0.01, 3),
    rep(0.10, 3),
    rep(0.40, 3)
  ),
  population_size = 10000
)

# ═══════════════════════════════════════════════════════════
# 7. sequentialtests_teaching: Educational Examples
# ═══════════════════════════════════════════════════════════
sequentialtests_teaching <- tibble(
  example_name = c(
    "Perfect Test 1",
    "Perfect Test 2",
    "Poor Test 1",
    "Poor Test 2",
    "High Sens Low Spec",
    "Low Sens High Spec",
    "Balanced Tests",
    "Identical Tests"
  ),
  test1_sens = c(1.00, 0.80, 0.60, 0.80, 0.95, 0.60, 0.85, 0.85),
  test1_spec = c(0.80, 1.00, 0.80, 0.60, 0.60, 0.95, 0.85, 0.85),
  test2_sens = c(0.80, 0.80, 0.80, 0.80, 0.80, 0.80, 0.85, 0.85),
  test2_spec = c(0.80, 0.80, 0.80, 0.80, 0.80, 0.80, 0.85, 0.85),
  prevalence = 0.10,
  concept_taught = c(
    "Perfect sensitivity effect",
    "Perfect specificity effect",
    "Poor sensitivity impact",
    "Poor specificity impact",
    "High sens/low spec trade-off",
    "Low sens/high spec trade-off",
    "Balanced performance",
    "No improvement from sequential"
  )
)

# ═══════════════════════════════════════════════════════════
# 8. sequentialtests_extreme: Edge Cases
# ═══════════════════════════════════════════════════════════
sequentialtests_extreme <- tibble(
  case_type = c(
    "Rare disease",
    "Common disease",
    "Very high sens",
    "Very high spec",
    "Very low sens",
    "Very low spec",
    "Expensive test 2",
    "Cheap test 2"
  ),
  test1_sens = c(0.90, 0.90, 0.99, 0.80, 0.50, 0.80, 0.85, 0.85),
  test1_spec = c(0.85, 0.85, 0.80, 0.99, 0.80, 0.50, 0.85, 0.85),
  test2_sens = c(0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.90, 0.90),
  test2_spec = c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95),
  test1_cost = c(50, 50, 50, 50, 50, 50, 50, 50),
  test2_cost = c(200, 200, 200, 200, 200, 200, 5000, 10),
  prevalence = c(0.001, 0.80, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10)
)

# ═══════════════════════════════════════════════════════════
# 9. sequentialtests_strategy_comparison: Direct Strategy Comparison
# ═══════════════════════════════════════════════════════════
# Same test parameters, different strategies
sequentialtests_strategy_comparison <- tibble(
  scenario_id = rep(1:5, each = 3),
  scenario_name = rep(c("Screening", "Diagnosis", "Emergency", "Rare disease", "Common disease"), each = 3),
  strategy = rep(c("serial_positive", "serial_negative", "parallel"), 5),
  test1_sens = rep(c(0.90, 0.85, 0.95, 0.92, 0.88), each = 3),
  test1_spec = rep(c(0.85, 0.88, 0.80, 0.90, 0.85), each = 3),
  test1_cost = rep(c(50, 100, 75, 30, 80), each = 3),
  test2_sens = rep(c(0.85, 0.90, 0.88, 0.85, 0.90), each = 3),
  test2_spec = rep(c(0.95, 0.95, 0.92, 0.98, 0.95), each = 3),
  test2_cost = rep(c(300, 400, 200, 500, 350), each = 3),
  prevalence = rep(c(0.05, 0.30, 0.15, 0.01, 0.60), each = 3),
  population_size = 10000
)

# ═══════════════════════════════════════════════════════════
# 10. sequentialtests_prevalence_sensitivity: Prevalence Impact Analysis
# ═══════════════════════════════════════════════════════════
prevalence_values <- c(0.001, 0.01, 0.05, 0.10, 0.20, 0.30, 0.50, 0.70, 0.90)

sequentialtests_prevalence_sensitivity <- tibble(
  prevalence = prevalence_values,
  test1_sens = 0.90,
  test1_spec = 0.85,
  test1_cost = 50,
  test2_sens = 0.85,
  test2_spec = 0.95,
  test2_cost = 250,
  strategy = "serial_positive",
  prevalence_category = case_when(
    prevalence < 0.01 ~ "Very rare",
    prevalence < 0.05 ~ "Rare",
    prevalence < 0.20 ~ "Uncommon",
    prevalence < 0.50 ~ "Common",
    TRUE ~ "Very common"
  )
)

# ═══════════════════════════════════════════════════════════
# 11. sequentialtests_preset_examples: All Clinical Presets
# ═══════════════════════════════════════════════════════════
sequentialtests_preset_examples <- tibble(
  preset_name = c(
    "covid_screening_confirmation",
    "breast_cancer_screening",
    "mi_emergency_parallel",
    "tb_screening_confirmation",
    "prostate_screening_exclusion",
    "hiv_screening_confirmation",
    "stroke_emergency_parallel"
  ),
  description = c(
    "COVID-19: Rapid antigen followed by RT-PCR",
    "Breast cancer: Mammography followed by biopsy",
    "MI: Troponin and ECG in parallel",
    "TB: Chest X-ray followed by culture",
    "Prostate: PSA followed by MRI for exclusion",
    "HIV: ELISA followed by Western blot",
    "Stroke: Clinical assessment and CT in parallel"
  ),
  test1_name = c("Rapid Antigen", "Mammography", "Troponin", "Chest X-ray", "PSA", "ELISA", "Clinical"),
  test1_sens = c(0.85, 0.87, 0.95, 0.70, 0.90, 0.995, 0.85),
  test1_spec = c(0.95, 0.88, 0.85, 0.92, 0.70, 0.995, 0.75),
  test1_cost = c(5, 100, 50, 30, 40, 30, 0),
  test2_name = c("RT-PCR", "Biopsy", "ECG", "Culture", "MRI", "Western Blot", "CT"),
  test2_sens = c(0.95, 0.95, 0.90, 0.85, 0.85, 0.999, 0.92),
  test2_spec = c(0.99, 0.98, 0.92, 0.98, 0.88, 0.999, 0.95),
  test2_cost = c(100, 500, 100, 80, 1200, 200, 800),
  prevalence = c(0.05, 0.006, 0.15, 0.02, 0.25, 0.001, 0.10),
  strategy = c("serial_positive", "serial_positive", "parallel", "serial_positive", "serial_negative", "serial_positive", "parallel")
)

# ═══════════════════════════════════════════════════════════
# Save datasets in multiple formats
# ═══════════════════════════════════════════════════════════
datasets <- list(
  sequentialtests_reference = sequentialtests_reference,
  sequentialtests_covid = sequentialtests_covid,
  sequentialtests_cancer = sequentialtests_cancer,
  sequentialtests_emergency = sequentialtests_emergency,
  sequentialtests_infectious = sequentialtests_infectious,
  sequentialtests_cost_comparison = sequentialtests_cost_comparison,
  sequentialtests_teaching = sequentialtests_teaching,
  sequentialtests_extreme = sequentialtests_extreme,
  sequentialtests_strategy_comparison = sequentialtests_strategy_comparison,
  sequentialtests_prevalence_sensitivity = sequentialtests_prevalence_sensitivity,
  sequentialtests_preset_examples = sequentialtests_preset_examples
)

# Save as RDA
for (name in names(datasets)) {
  assign(name, datasets[[name]])
  save(list = name, file = here("data", paste0(name, ".rda")), compress = "xz")
}

# Save as CSV
for (name in names(datasets)) {
  write.csv(datasets[[name]],
            file = here("data", paste0(name, ".csv")),
            row.names = FALSE)
}

# Save as XLSX
for (name in names(datasets)) {
  write_xlsx(datasets[[name]],
             path = here("data", paste0(name, ".xlsx")))
}

# Save as OMV (jamovi format)
for (name in names(datasets)) {
  write_omv(datasets[[name]],
            here("data", paste0(name, ".omv")))
}

cat("✓ Generated 11 reference datasets for sequentialtests function\n")
cat("✓ Created", length(datasets) * 4, "files (RDA, CSV, XLSX, OMV formats)\n")
cat("\nDatasets:\n")
for (name in names(datasets)) {
  cat(sprintf("  - %s (n=%d)\n", name, nrow(datasets[[name]])))
}
