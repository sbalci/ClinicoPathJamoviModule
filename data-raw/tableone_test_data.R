# ═══════════════════════════════════════════════════════════
# Test Data Generation: tableone
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic clinicopathological test data for the
# tableone jamovi function
#
# Generated: 2026-01-02
# Seed: 42
# Observations: 150
#
# Purpose: Create a comprehensive dataset that tests all tableone features
# including continuous variables, categorical variables, ordinal variables,
# and realistic missing data patterns.

library(tibble)
library(dplyr)
library(here)

# Set seed for reproducibility
set.seed(42)

# Sample size
n <- 150

# ═══════════════════════════════════════════════════════════
# Generate Clinicopathological Dataset
# ═══════════════════════════════════════════════════════════

tableone_test <- tibble(
  # ─────────────────────────────────────────────────────────
  # Patient Demographics
  # ─────────────────────────────────────────────────────────

  # Patient ID
  PatientID = sprintf("PT%03d", 1:n),

  # Age (years, realistic range 30-85)
  Age = round(rnorm(n, mean = 62, sd = 12)),

  # Sex (Male/Female with realistic distribution)
  Sex = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)),

  # ─────────────────────────────────────────────────────────
  # Clinical Variables
  # ─────────────────────────────────────────────────────────

  # Tumor Size (cm, 0.5-15 cm)
  TumorSize = pmax(0.5, rnorm(n, mean = 4.5, sd = 2.5)),

  # Tumor Stage (I-IV, ordinal)
  TumorStage = sample(c("I", "II", "III", "IV"), n, replace = TRUE,
                      prob = c(0.25, 0.35, 0.25, 0.15)),

  # Tumor Grade (1-3, ordinal)
  Grade = sample(c("1", "2", "3"), n, replace = TRUE,
                 prob = c(0.20, 0.50, 0.30)),

  # Lymph Node Status (Positive/Negative)
  LymphNodes = sample(c("Negative", "Positive"), n, replace = TRUE,
                      prob = c(0.60, 0.40)),

  # ─────────────────────────────────────────────────────────
  # Histopathological Features
  # ─────────────────────────────────────────────────────────

  # Lymphovascular Invasion (LVI)
  LVI = sample(c("Absent", "Present"), n, replace = TRUE,
               prob = c(0.65, 0.35)),

  # Perineural Invasion (PNI)
  PNI = sample(c("Absent", "Present"), n, replace = TRUE,
               prob = c(0.70, 0.30)),

  # Preinvasive Component
  PreinvasiveComponent = sample(c("Absent", "Present"), n, replace = TRUE,
                                prob = c(0.55, 0.45)),

  # Tumor Type (categorical with multiple levels)
  TumorType = sample(c("Adenocarcinoma", "Squamous Cell", "Mixed", "Other"),
                     n, replace = TRUE,
                     prob = c(0.50, 0.30, 0.15, 0.05)),

  # ─────────────────────────────────────────────────────────
  # Laboratory Values
  # ─────────────────────────────────────────────────────────

  # Hemoglobin (g/dL)
  Hemoglobin = rnorm(n, mean = 12.5, sd = 2.0),

  # White Blood Cell Count (10^9/L)
  WBC = pmax(2, rnorm(n, mean = 8.5, sd = 3.0)),

  # Ki67 Proliferation Index (%)
  Ki67 = pmax(0, pmin(100, rnorm(n, mean = 35, sd = 20))),

  # CA19-9 Tumor Marker (U/mL, log-normal distribution)
  CA199 = exp(rnorm(n, mean = 3.5, sd = 1.5)),

  # ─────────────────────────────────────────────────────────
  # Treatment Variables
  # ─────────────────────────────────────────────────────────

  # Treatment Group
  Treatment = sample(c("Surgery Only", "Surgery + Chemotherapy",
                      "Surgery + Radiation", "Trimodal"),
                    n, replace = TRUE,
                    prob = c(0.30, 0.35, 0.20, 0.15)),

  # Response to Treatment (RECIST criteria)
  Response = sample(c("Complete Response", "Partial Response",
                     "Stable Disease", "Progressive Disease"),
                   n, replace = TRUE,
                   prob = c(0.15, 0.35, 0.30, 0.20)),

  # ─────────────────────────────────────────────────────────
  # Survival/Outcome Variables
  # ─────────────────────────────────────────────────────────

  # Follow-up Time (months)
  FollowUpMonths = pmax(1, rnorm(n, mean = 36, sd = 24)),

  # Vital Status
  VitalStatus = sample(c("Alive", "Deceased"), n, replace = TRUE,
                       prob = c(0.65, 0.35))
)

# ═══════════════════════════════════════════════════════════
# Add Realistic Correlations and Clinical Patterns
# ═══════════════════════════════════════════════════════════

tableone_test <- tableone_test %>%
  mutate(
    # Higher stage correlates with larger tumor size
    TumorSize = case_when(
      TumorStage == "IV" ~ TumorSize * 1.4,
      TumorStage == "III" ~ TumorSize * 1.2,
      TumorStage == "II" ~ TumorSize * 1.0,
      TRUE ~ TumorSize * 0.8
    ),

    # Higher grade correlates with higher Ki67
    Ki67 = case_when(
      Grade == "3" ~ pmin(100, Ki67 * 1.3),
      Grade == "2" ~ Ki67,
      TRUE ~ Ki67 * 0.7
    ),

    # Advanced stage correlates with positive lymph nodes
    LymphNodes = case_when(
      TumorStage %in% c("III", "IV") & runif(n) > 0.3 ~ "Positive",
      TRUE ~ LymphNodes
    ),

    # Age effects on hemoglobin (mild anemia in elderly)
    Hemoglobin = Hemoglobin - (Age - 62) * 0.02,

    # Round continuous variables appropriately
    Age = round(Age),
    TumorSize = round(TumorSize, 1),
    Hemoglobin = round(Hemoglobin, 1),
    WBC = round(WBC, 1),
    Ki67 = round(Ki67, 0),
    CA199 = round(CA199, 1),
    FollowUpMonths = round(FollowUpMonths, 1)
  )

# ═══════════════════════════════════════════════════════════
# Add Realistic Missing Data Patterns (~8% overall)
# ═══════════════════════════════════════════════════════════

# Missing Completely At Random (MCAR) - ~3%
n_mcar <- round(n * 0.03)
tableone_test$Hemoglobin[sample(n, n_mcar)] <- NA
tableone_test$WBC[sample(n, n_mcar)] <- NA

# Missing At Random (MAR) - Advanced stages have more missing lab values ~5%
advanced_stage_idx <- which(tableone_test$TumorStage %in% c("III", "IV"))
n_mar <- round(length(advanced_stage_idx) * 0.15)
if (n_mar > 0) {
  tableone_test$Ki67[sample(advanced_stage_idx, n_mar)] <- NA
  tableone_test$CA199[sample(advanced_stage_idx, n_mar)] <- NA
}

# Missing Not At Random (MNAR) - Very high CA19-9 values sometimes not recorded
high_ca199_idx <- which(tableone_test$CA199 > quantile(tableone_test$CA199, 0.95, na.rm = TRUE))
if (length(high_ca199_idx) > 0) {
  tableone_test$CA199[sample(high_ca199_idx, round(length(high_ca199_idx) * 0.3))] <- NA
}

# Occasional missing in categorical variables (~2%)
tableone_test$PreinvasiveComponent[sample(n, round(n * 0.02))] <- NA
tableone_test$Response[sample(n, round(n * 0.02))] <- NA

# ═══════════════════════════════════════════════════════════
# Convert Ordinal Variables to Ordered Factors
# ═══════════════════════════════════════════════════════════

tableone_test <- tableone_test %>%
  mutate(
    TumorStage = factor(TumorStage, levels = c("I", "II", "III", "IV"), ordered = TRUE),
    Grade = factor(Grade, levels = c("1", "2", "3"), ordered = TRUE)
  )

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# Ensure data directory exists
if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"), recursive = TRUE)
}

# 1. RDA format (native R) - preserves factors and attributes
save(tableone_test, file = here::here("data", "tableone_test.rda"))
cat("✓ Saved: data/tableone_test.rda\n")

# 2. CSV format - for universal compatibility
write.csv(tableone_test,
          file = here::here("data", "tableone_test.csv"),
          row.names = FALSE,
          na = "")
cat("✓ Saved: data/tableone_test.csv\n")

# 3. Excel format - clinician-friendly
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(tableone_test,
                      path = here::here("data", "tableone_test.xlsx"))
  cat("✓ Saved: data/tableone_test.xlsx\n")
} else {
  warning("Package 'writexl' not available. Skipping Excel export.")
}

# 4. Jamovi format (OMV) - for jamovi UI testing
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(tableone_test,
                          here::here("data", "tableone_test.omv"))
  cat("✓ Saved: data/tableone_test.omv\n")
} else {
  warning("Package 'jmvReadWrite' not available. Skipping OMV export.")
}

# ═══════════════════════════════════════════════════════════
# Dataset Documentation
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("Dataset: tableone_test\n")
cat("═══════════════════════════════════════════════════════════\n\n")
cat("Observations:", n, "\n")
cat("Variables:", ncol(tableone_test), "\n")
cat("Missing data: ~8% overall (MCAR, MAR, MNAR patterns)\n\n")

cat("Variable descriptions:\n")
cat("──────────────────────────────────────────────────────────\n")
cat("Demographics:\n")
cat("  • PatientID         : Patient identifier (character)\n")
cat("  • Age               : Patient age in years (numeric, 30-85)\n")
cat("  • Sex               : Patient sex (Male/Female)\n\n")

cat("Clinical Variables:\n")
cat("  • TumorSize         : Tumor size in cm (numeric, 0.5-15)\n")
cat("  • TumorStage        : Tumor stage I-IV (ordinal factor)\n")
cat("  • Grade             : Tumor grade 1-3 (ordinal factor)\n")
cat("  • LymphNodes        : Lymph node status (Negative/Positive)\n\n")

cat("Histopathology:\n")
cat("  • LVI               : Lymphovascular invasion (Absent/Present)\n")
cat("  • PNI               : Perineural invasion (Absent/Present)\n")
cat("  • PreinvasiveComponent : Preinvasive component (Absent/Present)\n")
cat("  • TumorType         : Tumor histological type (4 categories)\n\n")

cat("Laboratory Values:\n")
cat("  • Hemoglobin        : Hemoglobin level g/dL (numeric, ~3% missing)\n")
cat("  • WBC               : White blood cell count 10^9/L (numeric)\n")
cat("  • Ki67              : Ki67 proliferation index % (numeric, 0-100)\n")
cat("  • CA199             : CA19-9 tumor marker U/mL (numeric, log-normal)\n\n")

cat("Treatment:\n")
cat("  • Treatment         : Treatment modality (4 categories)\n")
cat("  • Response          : Treatment response RECIST (4 categories)\n\n")

cat("Outcomes:\n")
cat("  • FollowUpMonths    : Follow-up duration in months (numeric)\n")
cat("  • VitalStatus       : Patient vital status (Alive/Deceased)\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("Realistic Clinical Patterns:\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("✓ Higher stage → larger tumor size\n")
cat("✓ Higher grade → higher Ki67 proliferation\n")
cat("✓ Advanced stage → more likely positive lymph nodes\n")
cat("✓ Older age → slightly lower hemoglobin\n")
cat("✓ Missing data patterns: MCAR, MAR, MNAR\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("Usage Examples:\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("# Load data in R\n")
cat("data(tableone_test, package = 'ClinicoPath')\n\n")

cat("# Basic Table One (all variables)\n")
cat("ClinicoPath::tableone(\n")
cat("  data = tableone_test,\n")
cat("  vars = c('Age', 'Sex', 'TumorSize', 'TumorStage', 'Grade'),\n")
cat("  sty = 't1'\n")
cat(")\n\n")

cat("# Publication-ready gtsummary style\n")
cat("ClinicoPath::tableone(\n")
cat("  data = tableone_test,\n")
cat("  vars = c('Age', 'Sex', 'TumorStage', 'Grade', 'LVI', 'PNI'),\n")
cat("  sty = 't2',\n")
cat("  excl = TRUE\n")
cat(")\n\n")

cat("# Arsenal comprehensive table\n")
cat("ClinicoPath::tableone(\n")
cat("  data = tableone_test,\n")
cat("  vars = c('Age', 'Sex', 'TumorSize', 'Hemoglobin', 'Ki67'),\n")
cat("  sty = 't3'\n")
cat(")\n\n")

cat("# Janitor frequency tables\n")
cat("ClinicoPath::tableone(\n")
cat("  data = tableone_test,\n")
cat("  vars = c('Sex', 'TumorStage', 'Grade', 'LymphNodes'),\n")
cat("  sty = 't4'\n")
cat(")\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("Data Quality Summary:\n")
cat("═══════════════════════════════════════════════════════════\n")

# Calculate missing data statistics
missing_summary <- sapply(tableone_test, function(x) sum(is.na(x)))
missing_pct <- round(100 * missing_summary / n, 1)
vars_with_missing <- names(missing_summary)[missing_summary > 0]

if (length(vars_with_missing) > 0) {
  cat("\nVariables with missing data:\n")
  for (var in vars_with_missing) {
    cat(sprintf("  • %-25s : %3d missing (%4.1f%%)\n",
                var, missing_summary[var], missing_pct[var]))
  }
} else {
  cat("\n✓ No missing data\n")
}

cat("\nComplete cases:", sum(complete.cases(tableone_test)),
    sprintf("(%4.1f%%)\n", 100 * sum(complete.cases(tableone_test)) / n))

cat("\n═══════════════════════════════════════════════════════════\n")
cat("Generation complete!\n")
cat("═══════════════════════════════════════════════════════════\n")
