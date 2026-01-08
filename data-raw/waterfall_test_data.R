# ═══════════════════════════════════════════════════════════
# Test Data Generation: waterfall
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the waterfall jamovi function
# which creates waterfall and spider plots for tumor response analysis
#
# Generated: 2026-01-06
# Seed: 42
# Function: Treatment Response Analysis (RECIST criteria)

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══════════════════════════════════════════════════════════
# 1. BASIC WATERFALL TEST DATA (PERCENTAGE CHANGES)
# ═══════════════════════════════════════════════════════════
# Simple dataset with pre-calculated percentage changes
# Best for testing basic waterfall plot functionality

n_patients <- 30

waterfall_test <- tibble(
  # Patient identifiers
  patientID = paste0("PT", sprintf("%03d", 1:n_patients)),

  # Best response (percentage change from baseline)
  # Negative = tumor shrinkage (good), Positive = tumor growth (poor)
  # Range from -100% (complete response) to +150% (progressive disease)
  best_response = c(
    # Complete Response (CR): ≤ -100%
    -100, -100,
    # Partial Response (PR): -99% to -30%
    -85, -75, -65, -55, -45, -35, -32,
    # Stable Disease (SD): -29% to +19%
    -25, -20, -15, -10, -5, 0, 5, 10, 15, 18,
    # Progressive Disease (PD): ≥ +20%
    22, 28, 35, 45, 55, 70, 85, 100, 120, 150, 180
  ),

  # Treatment group (for group-based coloring)
  treatment = sample(
    c("Monotherapy", "Combination", "Control"),
    size = n_patients,
    replace = TRUE,
    prob = c(0.4, 0.4, 0.2)
  ),

  # Disease subtype
  disease_subtype = sample(
    c("Type A", "Type B", "Type C"),
    size = n_patients,
    replace = TRUE
  ),

  # Prior therapy lines
  prior_lines = sample(0:3, n_patients, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
)

# Add realistic correlation: Combination therapy → better response
waterfall_test <- waterfall_test %>%
  mutate(
    best_response = case_when(
      treatment == "Combination" ~ best_response - 15,  # Better response
      treatment == "Control" ~ best_response + 10,      # Worse response
      TRUE ~ best_response
    ),
    # Cap at realistic values
    best_response = pmax(-100, pmin(200, best_response))
  )

# ═══════════════════════════════════════════════════════════
# 2. LONGITUDINAL DATA (FOR SPIDER PLOTS)
# ═══════════════════════════════════════════════════════════
# Time-series data for spider plot testing

n_patients_spider <- 20
timepoints <- c(0, 2, 4, 6, 8, 12)  # Baseline and follow-up months

# Generate baseline data
spider_baseline <- tibble(
  patientID = paste0("PT", sprintf("%03d", 1:n_patients_spider)),
  treatment = sample(c("Experimental", "Standard"), n_patients_spider, replace = TRUE),
  # Initial response trajectory slope (negative = good responder)
  response_trajectory = rnorm(n_patients_spider, mean = 0, sd = 15)
)

# Generate longitudinal measurements
waterfall_spider_test <- spider_baseline %>%
  tidyr::crossing(time = timepoints) %>%
  arrange(patientID, time) %>%
  mutate(
    # Calculate response at each timepoint
    pct_change = case_when(
      time == 0 ~ 0,  # Baseline = 0% change
      TRUE ~ response_trajectory +
        rnorm(n(), mean = time * 2, sd = 5) +  # Add time effect + noise
        ifelse(treatment == "Experimental", -10, 5)  # Treatment effect
    ),
    # Add realistic variability and limits
    pct_change = pmax(-100, pmin(200, pct_change)),

    # Determine response category at each timepoint
    response_category = case_when(
      pct_change <= -100 ~ "CR",
      pct_change <= -30 ~ "PR",
      pct_change <= 20 ~ "SD",
      TRUE ~ "PD"
    )
  ) %>%
  select(-response_trajectory)

# ═══════════════════════════════════════════════════════════
# 3. RAW MEASUREMENTS DATA
# ═══════════════════════════════════════════════════════════
# Actual tumor measurements (sum of diameters in mm)

n_patients_raw <- 25

waterfall_raw_test <- tibble(
  patientID = rep(paste0("PT", sprintf("%03d", 1:n_patients_raw)), each = 5),
  time = rep(c(0, 1, 2, 4, 6), times = n_patients_raw),  # months
  treatment = rep(
    sample(c("Drug A", "Drug B", "Placebo"), n_patients_raw, replace = TRUE),
    each = 5
  )
) %>%
  group_by(patientID) %>%
  mutate(
    # Generate baseline tumor size (30-150 mm)
    baseline_size = first(runif(1, min = 30, max = 150)),

    # Generate response pattern
    response_rate = case_when(
      treatment == "Drug A" ~ -0.08,  # 8% shrinkage per month
      treatment == "Drug B" ~ -0.05,  # 5% shrinkage per month
      TRUE ~ 0.02  # 2% growth per month
    ),

    # Calculate tumor size at each timepoint
    tumor_size = baseline_size * exp(response_rate * time + rnorm(n(), 0, 0.1)),
    tumor_size = pmax(0, tumor_size),  # Cannot be negative

    # Round to realistic values
    tumor_size = round(tumor_size, 1)
  ) %>%
  ungroup() %>%
  select(patientID, time, tumor_size, treatment)

# ═══════════════════════════════════════════════════════════
# 4. EDGE CASES AND SPECIAL SCENARIOS
# ═══════════════════════════════════════════════════════════

# Small dataset (minimal viable)
waterfall_small <- tibble(
  patientID = paste0("PT", sprintf("%03d", 1:5)),
  best_response = c(-80, -40, -10, 15, 60),
  treatment = c("A", "A", "B", "B", "Control")
)

# Large dataset (performance testing)
n_large <- 200
waterfall_large <- tibble(
  patientID = paste0("PT", sprintf("%04d", 1:n_large)),
  best_response = rnorm(n_large, mean = -10, sd = 40),
  treatment = sample(paste0("Arm_", LETTERS[1:8]), n_large, replace = TRUE),
  biomarker_status = sample(c("Positive", "Negative"), n_large, replace = TRUE)
) %>%
  mutate(best_response = pmax(-100, pmin(300, best_response)))

# Dataset with missing values
waterfall_missing <- waterfall_test %>%
  slice(1:20) %>%
  mutate(
    best_response = ifelse(row_number() %in% c(3, 7, 15), NA, best_response),
    treatment = ifelse(row_number() %in% c(5, 12), NA, treatment)
  )

# Dataset with extreme values
waterfall_extreme <- tibble(
  patientID = paste0("PT", sprintf("%03d", 1:15)),
  best_response = c(
    -100, -100, -100,  # Multiple complete responses
    -95, -85, -75,     # Strong partial responses
    -30, -20, -10, 0,  # Borderline PR/SD
    20, 25, 30,        # Borderline SD/PD
    200, 350           # Extreme progressive disease
  ),
  treatment = rep(c("A", "B", "C"), length.out = 15)
)

# Dataset with baseline issues (for error testing)
waterfall_no_baseline <- waterfall_spider_test %>%
  filter(time != 0) %>%  # Remove all baseline measurements
  slice(1:20)

# ═══════════════════════════════════════════════════════════
# 5. CLINICAL TRIAL REALISTIC DATA
# ═══════════════════════════════════════════════════════════
# Mimics Phase II oncology trial

n_phase2 <- 50
waterfall_phase2 <- tibble(
  patientID = paste0("STUDY001-", sprintf("%03d", 1:n_phase2)),

  # Realistic response distribution for immunotherapy
  # ORR ~30%, DCR ~60%
  best_response = c(
    # CR (4%)
    rep(-100, 2),
    # PR (26%)
    rnorm(13, mean = -50, sd = 15),
    # SD (30%)
    rnorm(15, mean = 0, sd = 10),
    # PD (40%)
    rnorm(20, mean = 50, sd = 30)
  ),

  # Study arms
  cohort = sample(
    c("Dose Level 1", "Dose Level 2", "Dose Level 3"),
    n_phase2, replace = TRUE
  ),

  # Baseline characteristics
  age = round(rnorm(n_phase2, mean = 62, sd = 10)),
  ecog_ps = sample(0:2, n_phase2, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  pdl1_status = sample(c("<1%", "1-49%", "≥50%"), n_phase2, replace = TRUE),

  # Time on treatment (months)
  time_on_treatment = pmax(1, rnorm(n_phase2, mean = 6, sd = 3))
) %>%
  mutate(
    best_response = pmax(-100, pmin(200, best_response)),
    # Better response with higher PD-L1
    best_response = case_when(
      pdl1_status == "≥50%" ~ best_response - 20,
      pdl1_status == "1-49%" ~ best_response - 10,
      TRUE ~ best_response
    ),
    best_response = pmax(-100, best_response)
  )

# ═══════════════════════════════════════════════════════════
# SAVE ALL DATASETS
# ═══════════════════════════════════════════════════════════

# Main test datasets
datasets <- list(
  waterfall_test = waterfall_test,
  waterfall_spider_test = waterfall_spider_test,
  waterfall_raw_test = waterfall_raw_test,
  waterfall_small = waterfall_small,
  waterfall_large = waterfall_large,
  waterfall_missing = waterfall_missing,
  waterfall_extreme = waterfall_extreme,
  waterfall_no_baseline = waterfall_no_baseline,
  waterfall_phase2 = waterfall_phase2
)

# Save each dataset in multiple formats
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]

  # 1. RDA format (native R)
  save_name <- dataset_name
  assign(save_name, data)
  save(list = save_name,
       file = here::here("data", paste0(dataset_name, ".rda")),
       compress = "xz")

  # 2. CSV format
  write.csv(data,
            file = here::here("data", paste0(dataset_name, ".csv")),
            row.names = FALSE)

  # 3. Excel format
  writexl::write_xlsx(data,
                      path = here::here("data", paste0(dataset_name, ".xlsx")))

  # 4. Jamovi format (OMV)
  jmvReadWrite::write_omv(dtaFrm = data,
                          fleOut = here::here("data", paste0(dataset_name, ".omv")))
}

# ═══════════════════════════════════════════════════════════
# DOCUMENTATION
# ═══════════════════════════════════════════════════════════

cat("
╔════════════════════════════════════════════════════════════╗
║       Waterfall Test Data Generation Summary              ║
╚════════════════════════════════════════════════════════════╝

DATASETS CREATED:
─────────────────────────────────────────────────────────────

1. waterfall_test (Main test dataset)
   • Observations: 30 patients
   • Variables: 5 (patientID, best_response, treatment, disease_subtype, prior_lines)
   • Format: Pre-calculated percentage changes
   • Use case: Basic waterfall plot testing
   • Response distribution: CR=2, PR=7, SD=9, PD=12

2. waterfall_spider_test (Longitudinal data)
   • Observations: 120 (20 patients × 6 timepoints)
   • Variables: 4 (patientID, treatment, time, pct_change)
   • Format: Time-series percentage changes
   • Use case: Spider plot testing
   • Timepoints: 0, 2, 4, 6, 8, 12 months

3. waterfall_raw_test (Raw measurements)
   • Observations: 125 (25 patients × 5 timepoints)
   • Variables: 4 (patientID, time, tumor_size, treatment)
   • Format: Actual tumor measurements (mm)
   • Use case: Raw measurement processing
   • Timepoints: 0, 1, 2, 4, 6 months

4. waterfall_small (Minimal dataset)
   • Observations: 5 patients
   • Variables: 3
   • Use case: Edge case testing

5. waterfall_large (Performance testing)
   • Observations: 200 patients
   • Variables: 4
   • Use case: Large dataset handling

6. waterfall_missing (Missing data)
   • Observations: 20 patients
   • Variables: 3
   • Missing: ~15% in best_response and treatment
   • Use case: Missing data handling

7. waterfall_extreme (Extreme values)
   • Observations: 15 patients
   • Variables: 3
   • Includes: Multiple CRs, extreme PD (>200%)
   • Use case: Outlier handling

8. waterfall_no_baseline (Error testing)
   • Observations: 20 measurements
   • Variables: 4
   • Missing: All baseline (time=0) measurements
   • Use case: Validation error testing

9. waterfall_phase2 (Clinical trial)
   • Observations: 50 patients
   • Variables: 7
   • Use case: Realistic Phase II trial simulation
   • ORR: ~30%, DCR: ~60%

─────────────────────────────────────────────────────────────
FILE FORMATS:
─────────────────────────────────────────────────────────────
Each dataset saved in 4 formats:
  ✓ .rda   - R data format (fast loading)
  ✓ .csv   - CSV format (universal)
  ✓ .xlsx  - Excel format (clinician-friendly)
  ✓ .omv   - Jamovi format (native)

─────────────────────────────────────────────────────────────
VARIABLE DESCRIPTIONS:
─────────────────────────────────────────────────────────────

patientID        : Unique patient identifier
best_response    : Percentage change from baseline (negative = shrinkage)
time            : Time from baseline (months)
tumor_size      : Sum of target lesion diameters (mm)
treatment       : Treatment group/arm
pct_change      : Percentage change at each timepoint
response_category: RECIST category (CR/PR/SD/PD)

RECIST v1.1 THRESHOLDS:
  CR (Complete Response)    : ≤ -100%
  PR (Partial Response)     : -99% to -30%
  SD (Stable Disease)       : -29% to +19%
  PD (Progressive Disease)  : ≥ +20%

─────────────────────────────────────────────────────────────
USAGE EXAMPLES:
─────────────────────────────────────────────────────────────

# Load test data
data(waterfall_test, package = 'ClinicoPath')

# Basic waterfall plot
waterfall(
  data = waterfall_test,
  patientID = 'patientID',
  responseVar = 'best_response',
  inputType = 'percentage'
)

# Waterfall with grouping
waterfall(
  data = waterfall_test,
  patientID = 'patientID',
  responseVar = 'best_response',
  groupVar = 'treatment',
  colorBy = 'group'
)

# Spider plot (longitudinal)
waterfall(
  data = waterfall_spider_test,
  patientID = 'patientID',
  responseVar = 'pct_change',
  timeVar = 'time',
  showSpiderPlot = TRUE
)

# Raw measurements with automatic calculation
waterfall(
  data = waterfall_raw_test,
  patientID = 'patientID',
  responseVar = 'tumor_size',
  timeVar = 'time',
  inputType = 'raw',
  groupVar = 'treatment'
)

# Clinical trial analysis
waterfall(
  data = waterfall_phase2,
  patientID = 'patientID',
  responseVar = 'best_response',
  groupVar = 'cohort',
  generateCopyReadyReport = TRUE,
  showConfidenceIntervals = TRUE
)

─────────────────────────────────────────────────────────────
Generated: 2026-01-06
Seed: 42
Package: ClinicoPath
─────────────────────────────────────────────────────────────
")
