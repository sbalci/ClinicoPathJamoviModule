# ═══════════════════════════════════════════════════════════
# Test Data Generation: lollipop
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic clinical lab test data for the lollipop jamovi function
#
# Generated: 2026-01-05
# Seed: 42
# Purpose: Clinical laboratory data visualization with lollipop charts

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# ═══════════════════════════════════════════════════════════
# Dataset 1: lollipop_test - Comprehensive Clinical Lab Data
# ═══════════════════════════════════════════════════════════

n <- 120  # 30 patients × 4 treatment groups

lollipop_test <- tibble(
  patient_id = rep(1:30, each = 4),

  # Treatment groups
  treatment_group = rep(c("Control", "Drug A", "Drug B", "Drug C"), times = 30),

  # Disease severity
  disease_severity = sample(c("Mild", "Moderate", "Severe"), n, replace = TRUE,
                            prob = c(0.3, 0.4, 0.3)),

  # Age groups
  age_group = sample(c("18-40", "41-60", "61-80", "80+"), n, replace = TRUE,
                     prob = c(0.2, 0.3, 0.4, 0.1)),

  # Hospital
  hospital = sample(c("Hospital A", "Hospital B", "Hospital C"), n, replace = TRUE),

  # Clinical lab values - realistic ranges

  # Hemoglobin (g/dL) - Normal: 12-16 for women, 14-18 for men
  hemoglobin = rnorm(n, mean = 13.5, sd = 2),

  # Albumin (g/dL) - Normal: 3.5-5.5
  albumin = rnorm(n, mean = 4.2, sd = 0.8),

  # Creatinine (mg/dL) - Normal: 0.6-1.2
  creatinine = rnorm(n, mean = 1.0, sd = 0.3),

  # Platelet count (×10³/μL) - Normal: 150-450
  platelet_count = rnorm(n, mean = 250, sd = 80),

  # White blood cells (×10³/μL) - Normal: 4-11
  white_blood_cells = rnorm(n, mean = 7.5, sd = 2.5),

  # ALT (U/L) - Normal: <40
  alt = rnorm(n, mean = 30, sd = 15),

  # CRP (mg/L) - Normal: <10
  crp = abs(rnorm(n, mean = 8, sd = 12))
)

# Add realistic treatment effects
lollipop_test <- lollipop_test %>%
  mutate(
    # Drug A improves hemoglobin
    hemoglobin = case_when(
      treatment_group == "Drug A" ~ hemoglobin + 1.5,
      treatment_group == "Drug B" ~ hemoglobin + 0.8,
      TRUE ~ hemoglobin
    ),

    # Severe disease → lower albumin
    albumin = case_when(
      disease_severity == "Severe" ~ albumin - 1.0,
      disease_severity == "Moderate" ~ albumin - 0.4,
      TRUE ~ albumin
    ),

    # Drug C reduces CRP
    crp = case_when(
      treatment_group == "Drug C" ~ crp * 0.6,
      TRUE ~ crp
    )
  )

# Add some missing data (~3%)
n_missing <- round(n * 0.03)
lollipop_test$hemoglobin[sample(n, n_missing)] <- NA
lollipop_test$albumin[sample(n, n_missing)] <- NA

# Ensure positive values for counts
lollipop_test <- lollipop_test %>%
  mutate(
    platelet_count = pmax(50, platelet_count),
    white_blood_cells = pmax(1, white_blood_cells),
    alt = pmax(5, alt),
    crp = pmax(0.1, crp)
  )

# Save in multiple formats
save(lollipop_test, file = here("data", "lollipop_test.rda"))
write.csv(lollipop_test, file = here("data", "lollipop_test.csv"), row.names = FALSE)
write_xlsx(lollipop_test, path = here("data", "lollipop_test.xlsx"))
write_omv(lollipop_test, here("data", "lollipop_test.omv"))

cat("✅ lollipop_test: n =", n, "| Clinical lab data with multiple parameters\n")


# ═══════════════════════════════════════════════════════════
# Dataset 2: lollipop_treatment - Treatment Comparison
# ═══════════════════════════════════════════════════════════

n_treat <- 40  # 10 patients per treatment

lollipop_treatment <- tibble(
  patient = paste0("P", 1:n_treat),

  treatment = rep(c("Placebo", "Low Dose", "Medium Dose", "High Dose"), each = 10),

  # Primary outcome - tumor size reduction (%)
  tumor_reduction = c(
    rnorm(10, mean = 5, sd = 8),    # Placebo
    rnorm(10, mean = 15, sd = 10),  # Low dose
    rnorm(10, mean = 35, sd = 12),  # Medium dose
    rnorm(10, mean = 55, sd = 15)   # High dose
  ),

  # Secondary outcome - quality of life score (0-100)
  qol_score = c(
    rnorm(10, mean = 60, sd = 12),
    rnorm(10, mean = 65, sd = 10),
    rnorm(10, mean = 72, sd = 8),
    rnorm(10, mean = 78, sd = 10)
  ),

  # Side effect score (lower is better, 0-50)
  side_effects = c(
    rnorm(10, mean = 5, sd = 3),
    rnorm(10, mean = 10, sd = 4),
    rnorm(10, mean = 18, sd = 5),
    rnorm(10, mean = 28, sd = 6)
  )
)

# Ensure realistic bounds
lollipop_treatment <- lollipop_treatment %>%
  mutate(
    tumor_reduction = pmin(100, pmax(-20, tumor_reduction)),
    qol_score = pmin(100, pmax(0, qol_score)),
    side_effects = pmin(50, pmax(0, side_effects))
  )

save(lollipop_treatment, file = here("data", "lollipop_treatment.rda"))
write.csv(lollipop_treatment, file = here("data", "lollipop_treatment.csv"), row.names = FALSE)
write_xlsx(lollipop_treatment, path = here("data", "lollipop_treatment.xlsx"))
write_omv(lollipop_treatment, here("data", "lollipop_treatment.omv"))

cat("✅ lollipop_treatment: n =", n_treat, "| Treatment dose-response data\n")


# ═══════════════════════════════════════════════════════════
# Dataset 3: lollipop_biomarkers - Biomarker Panel
# ═══════════════════════════════════════════════════════════

n_bio <- 60  # 20 patients × 3 cancer types

lollipop_biomarkers <- tibble(
  patient_id = paste0("PT", 1:n_bio),

  cancer_type = rep(c("Type A", "Type B", "Type C"), each = 20),

  # Panel of tumor biomarkers (ng/mL)
  cea = abs(rnorm(n_bio, mean = 5, sd = 8)),      # CEA
  ca125 = abs(rnorm(n_bio, mean = 30, sd = 40)),  # CA-125
  ca199 = abs(rnorm(n_bio, mean = 25, sd = 35)),  # CA 19-9
  afp = abs(rnorm(n_bio, mean = 10, sd = 15)),    # AFP
  psa = abs(rnorm(n_bio, mean = 4, sd = 6))       # PSA
)

# Add cancer type-specific elevations
lollipop_biomarkers <- lollipop_biomarkers %>%
  mutate(
    cea = case_when(
      cancer_type == "Type A" ~ cea * 3,
      TRUE ~ cea
    ),
    ca125 = case_when(
      cancer_type == "Type B" ~ ca125 * 4,
      TRUE ~ ca125
    ),
    ca199 = case_when(
      cancer_type == "Type C" ~ ca199 * 3.5,
      TRUE ~ ca199
    )
  )

save(lollipop_biomarkers, file = here("data", "lollipop_biomarkers.rda"))
write.csv(lollipop_biomarkers, file = here("data", "lollipop_biomarkers.csv"), row.names = FALSE)
write_xlsx(lollipop_biomarkers, path = here("data", "lollipop_biomarkers.xlsx"))
write_omv(lollipop_biomarkers, here("data", "lollipop_biomarkers.omv"))

cat("✅ lollipop_biomarkers: n =", n_bio, "| Tumor biomarker panel data\n")


# ═══════════════════════════════════════════════════════════
# Dataset 4: lollipop_hospital - Multi-center Comparison
# ═══════════════════════════════════════════════════════════

n_hosp <- 50  # 10 hospitals × ~5 metrics each

lollipop_hospital <- tibble(
  hospital = paste0("Hospital ", LETTERS[1:10]),

  # Quality metrics
  survival_rate = rnorm(10, mean = 85, sd = 8),
  complication_rate = rnorm(10, mean = 12, sd = 5),
  readmission_rate = rnorm(10, mean = 15, sd = 6),
  patient_satisfaction = rnorm(10, mean = 78, sd = 10),
  wait_time_days = abs(rnorm(10, mean = 14, sd = 8))
)

# Ensure realistic bounds
lollipop_hospital <- lollipop_hospital %>%
  mutate(
    survival_rate = pmin(100, pmax(60, survival_rate)),
    complication_rate = pmin(30, pmax(2, complication_rate)),
    readmission_rate = pmin(40, pmax(5, readmission_rate)),
    patient_satisfaction = pmin(100, pmax(40, patient_satisfaction)),
    wait_time_days = pmin(60, pmax(1, wait_time_days))
  )

save(lollipop_hospital, file = here("data", "lollipop_hospital.rda"))
write.csv(lollipop_hospital, file = here("data", "lollipop_hospital.csv"), row.names = FALSE)
write_xlsx(lollipop_hospital, path = here("data", "lollipop_hospital.xlsx"))
write_omv(lollipop_hospital, here("data", "lollipop_hospital.omv"))

cat("✅ lollipop_hospital: n =", nrow(lollipop_hospital), "| Hospital quality metrics\n")


# ═══════════════════════════════════════════════════════════
# Dataset 5: lollipop_small - Small Sample
# ═══════════════════════════════════════════════════════════

n_small <- 15  # 5 groups × 3 replicates

lollipop_small <- tibble(
  sample = paste0("S", 1:n_small),

  category = rep(c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"), each = 3),

  measurement = c(
    rnorm(3, mean = 50, sd = 5),
    rnorm(3, mean = 65, sd = 6),
    rnorm(3, mean = 45, sd = 4),
    rnorm(3, mean = 80, sd = 7),
    rnorm(3, mean = 55, sd = 5)
  )
)

save(lollipop_small, file = here("data", "lollipop_small.rda"))
write.csv(lollipop_small, file = here("data", "lollipop_small.csv"), row.names = FALSE)
write_xlsx(lollipop_small, path = here("data", "lollipop_small.xlsx"))
write_omv(lollipop_small, here("data", "lollipop_small.omv"))

cat("✅ lollipop_small: n =", n_small, "| Small sample test data\n")


# ═══════════════════════════════════════════════════════════
# Dataset 6: lollipop_aggregated - Pre-aggregated Summary Data
# ═══════════════════════════════════════════════════════════

lollipop_aggregated <- tibble(
  region = c("North", "South", "East", "West", "Central"),

  mean_income = c(75000, 62000, 88000, 71000, 69000),
  median_age = c(42, 38, 45, 41, 39),
  population_density = c(850, 420, 1200, 680, 920),
  unemployment_rate = c(5.2, 6.8, 4.1, 5.9, 5.5)
)

save(lollipop_aggregated, file = here("data", "lollipop_aggregated.rda"))
write.csv(lollipop_aggregated, file = here("data", "lollipop_aggregated.csv"), row.names = FALSE)
write_xlsx(lollipop_aggregated, path = here("data", "lollipop_aggregated.xlsx"))
write_omv(lollipop_aggregated, here("data", "lollipop_aggregated.omv"))

cat("✅ lollipop_aggregated: n =", nrow(lollipop_aggregated), "| Pre-aggregated regional data\n")


# ═══════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("Test Data Generation Complete\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Datasets created:\n")
cat("  1. lollipop_test (n=120)         - Comprehensive clinical lab data\n")
cat("  2. lollipop_treatment (n=40)     - Treatment dose-response\n")
cat("  3. lollipop_biomarkers (n=60)    - Tumor biomarker panel\n")
cat("  4. lollipop_hospital (n=10)      - Hospital quality metrics\n")
cat("  5. lollipop_small (n=15)         - Small sample test\n")
cat("  6. lollipop_aggregated (n=5)     - Pre-aggregated summary\n\n")

cat("Total files generated: 24 (6 datasets × 4 formats)\n\n")

cat("Formats:\n")
cat("  - RDA (R native)\n")
cat("  - CSV (universal)\n")
cat("  - XLSX (Excel)\n")
cat("  - OMV (jamovi native)\n\n")

cat("Usage:\n")
cat("  data(lollipop_test)\n")
cat("  lollipop(data = lollipop_test, dep = 'hemoglobin', group = 'treatment_group')\n\n")
