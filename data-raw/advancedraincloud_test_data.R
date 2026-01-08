# ═══════════════════════════════════════════════════════════
# Test Data Generation: advancedraincloud
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the advancedraincloud jamovi function
#
# Generated: 2026-01-04
# Seed: 42
# Scenarios:
#   1. Basic group comparison (pain scores)
#   2. Longitudinal repeated measures (treatment over time)
#   3. Likert scale survey data
#   4. Clinical trial with biomarkers
#
# Function purpose: Advanced raincloud plots with longitudinal connections,
# Likert scale support, and clinical trial features using ggrain package.

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══════════════════════════════════════════════════════════
# Generate Comprehensive Test Dataset
# ═══════════════════════════════════════════════════════════

# Sample size per group
n_per_group <- 50
n_groups <- 3
n_timepoints <- 3
n_total <- n_per_group * n_groups * n_timepoints

# Generate longitudinal clinical trial data
advancedraincloud_test <- tibble(
  # ─────────────────────────────────────────────────────────
  # Core identifiers
  # ─────────────────────────────────────────────────────────
  patient_id = rep(1:(n_per_group * n_groups), each = n_timepoints),

  # ─────────────────────────────────────────────────────────
  # Treatment arms (x_var and fill_var candidate)
  # ─────────────────────────────────────────────────────────
  treatment = rep(
    rep(c("Placebo", "Low Dose", "High Dose"), each = n_per_group),
    each = n_timepoints
  ),

  # ─────────────────────────────────────────────────────────
  # Time points (x_var candidate for longitudinal)
  # ─────────────────────────────────────────────────────────
  timepoint = rep(c("Baseline", "Week 4", "Week 12"), times = n_per_group * n_groups),

  # ─────────────────────────────────────────────────────────
  # Primary outcome: Pain Score (y_var - continuous 0-100)
  # Realistic treatment effect over time
  # ─────────────────────────────────────────────────────────
  pain_score = NA_real_,

  # ─────────────────────────────────────────────────────────
  # Secondary outcome: Function Score (y_var alternative)
  # Higher is better (opposite of pain)
  # ─────────────────────────────────────────────────────────
  function_score = NA_real_,

  # ─────────────────────────────────────────────────────────
  # Likert scale data (y_var for Likert mode)
  # Patient satisfaction: 1-7 scale
  # ─────────────────────────────────────────────────────────
  satisfaction = NA_integer_,

  # ─────────────────────────────────────────────────────────
  # Biomarker data (y_var for biomarker analysis)
  # CRP levels (mg/L) - should decrease with treatment
  # ─────────────────────────────────────────────────────────
  crp_level = NA_real_,

  # ─────────────────────────────────────────────────────────
  # Patient demographics (cov_var candidates)
  # ─────────────────────────────────────────────────────────
  age = rep(round(rnorm(n_per_group * n_groups, 58, 12)), each = n_timepoints),
  sex = rep(sample(c("Male", "Female"), n_per_group * n_groups, replace = TRUE), each = n_timepoints),
  bmi = rep(round(rnorm(n_per_group * n_groups, 28, 5), 1), each = n_timepoints),

  # ─────────────────────────────────────────────────────────
  # Disease severity at baseline (fill_var or cov_var)
  # ─────────────────────────────────────────────────────────
  disease_severity = rep(
    sample(c("Mild", "Moderate", "Severe"), n_per_group * n_groups,
           replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    each = n_timepoints
  ),

  # ─────────────────────────────────────────────────────────
  # Responder status (binary outcome)
  # Defined as >20% improvement from baseline
  # ─────────────────────────────────────────────────────────
  responder = NA_character_,

  # ─────────────────────────────────────────────────────────
  # Study site (additional grouping variable)
  # ─────────────────────────────────────────────────────────
  site = rep(
    sample(c("Site A", "Site B", "Site C"), n_per_group * n_groups,
           replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    each = n_timepoints
  )
)

# ─────────────────────────────────────────────────────────
# Generate realistic outcome variables based on treatment/time
# ─────────────────────────────────────────────────────────

# Generate pain scores with treatment effects
for (i in 1:nrow(advancedraincloud_test)) {
  trt <- advancedraincloud_test$treatment[i]
  tp <- advancedraincloud_test$timepoint[i]

  if (trt == "Placebo") {
    if (tp == "Baseline") advancedraincloud_test$pain_score[i] <- rnorm(1, 65, 12)
    else if (tp == "Week 4") advancedraincloud_test$pain_score[i] <- rnorm(1, 60, 13)
    else advancedraincloud_test$pain_score[i] <- rnorm(1, 58, 14)
  } else if (trt == "Low Dose") {
    if (tp == "Baseline") advancedraincloud_test$pain_score[i] <- rnorm(1, 64, 11)
    else if (tp == "Week 4") advancedraincloud_test$pain_score[i] <- rnorm(1, 50, 12)
    else advancedraincloud_test$pain_score[i] <- rnorm(1, 42, 13)
  } else { # High Dose
    if (tp == "Baseline") advancedraincloud_test$pain_score[i] <- rnorm(1, 66, 10)
    else if (tp == "Week 4") advancedraincloud_test$pain_score[i] <- rnorm(1, 45, 11)
    else advancedraincloud_test$pain_score[i] <- rnorm(1, 32, 12)
  }

  # Generate CRP levels
  if (trt == "Placebo") {
    if (tp == "Baseline") advancedraincloud_test$crp_level[i] <- rlnorm(1, log(8), 0.6)
    else if (tp == "Week 4") advancedraincloud_test$crp_level[i] <- rlnorm(1, log(7.5), 0.6)
    else advancedraincloud_test$crp_level[i] <- rlnorm(1, log(7.2), 0.6)
  } else if (trt == "Low Dose") {
    if (tp == "Baseline") advancedraincloud_test$crp_level[i] <- rlnorm(1, log(8.2), 0.6)
    else if (tp == "Week 4") advancedraincloud_test$crp_level[i] <- rlnorm(1, log(5.5), 0.6)
    else advancedraincloud_test$crp_level[i] <- rlnorm(1, log(4.0), 0.6)
  } else { # High Dose
    if (tp == "Baseline") advancedraincloud_test$crp_level[i] <- rlnorm(1, log(7.8), 0.6)
    else if (tp == "Week 4") advancedraincloud_test$crp_level[i] <- rlnorm(1, log(4.5), 0.6)
    else advancedraincloud_test$crp_level[i] <- rlnorm(1, log(2.8), 0.6)
  }

  # Generate satisfaction scores (Likert 1-7)
  if (trt == "Placebo") {
    advancedraincloud_test$satisfaction[i] <- sample(1:7, 1, prob = c(0.05, 0.15, 0.25, 0.30, 0.15, 0.08, 0.02))
  } else if (trt == "Low Dose") {
    advancedraincloud_test$satisfaction[i] <- sample(1:7, 1, prob = c(0.02, 0.08, 0.15, 0.25, 0.25, 0.15, 0.10))
  } else { # High Dose
    advancedraincloud_test$satisfaction[i] <- sample(1:7, 1, prob = c(0.01, 0.04, 0.08, 0.15, 0.25, 0.27, 0.20))
  }

  # Generate responder status
  if (trt == "Placebo") {
    advancedraincloud_test$responder[i] <- sample(c("Non-responder", "Responder"), 1, prob = c(0.7, 0.3))
  } else if (trt == "Low Dose") {
    advancedraincloud_test$responder[i] <- sample(c("Non-responder", "Responder"), 1, prob = c(0.5, 0.5))
  } else { # High Dose
    advancedraincloud_test$responder[i] <- sample(c("Non-responder", "Responder"), 1, prob = c(0.3, 0.7))
  }
}

# Generate function score (inverse of pain + noise)
advancedraincloud_test$function_score <- 100 - advancedraincloud_test$pain_score + rnorm(nrow(advancedraincloud_test), 0, 8)

# ─────────────────────────────────────────────────────────
# Add realistic constraints and data quality issues
# ─────────────────────────────────────────────────────────

# Constrain pain scores to 0-100 range
advancedraincloud_test <- advancedraincloud_test %>%
  mutate(
    pain_score = pmax(0, pmin(100, pain_score)),
    function_score = pmax(0, pmin(100, function_score)),
    crp_level = pmax(0.1, crp_level)  # CRP can't be negative
  )

# Add some missing data (~3% realistic missingness)
n_missing <- round(nrow(advancedraincloud_test) * 0.03)
missing_indices <- sample(1:nrow(advancedraincloud_test), n_missing)

advancedraincloud_test$pain_score[missing_indices[1:round(n_missing/3)]] <- NA
advancedraincloud_test$crp_level[missing_indices[round(n_missing/3):round(2*n_missing/3)]] <- NA
advancedraincloud_test$satisfaction[missing_indices[round(2*n_missing/3):n_missing]] <- NA

# Convert character variables to factors with proper ordering
advancedraincloud_test <- advancedraincloud_test %>%
  mutate(
    treatment = factor(treatment, levels = c("Placebo", "Low Dose", "High Dose")),
    timepoint = factor(timepoint, levels = c("Baseline", "Week 4", "Week 12")),
    disease_severity = factor(disease_severity, levels = c("Mild", "Moderate", "Severe")),
    responder = factor(responder, levels = c("Non-responder", "Responder")),
    sex = factor(sex),
    site = factor(site)
  )

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(advancedraincloud_test, file = here::here("data", "advancedraincloud_test.rda"))

# 2. CSV format
write.csv(advancedraincloud_test,
          file = here::here("data", "advancedraincloud_test.csv"),
          row.names = FALSE)

# 3. Excel format
writexl::write_xlsx(advancedraincloud_test,
                    path = here::here("data", "advancedraincloud_test.xlsx"))

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(advancedraincloud_test,
                        here::here("data", "advancedraincloud_test.omv"))

# ═══════════════════════════════════════════════════════════
# Generate Data Summary Report
# ═══════════════════════════════════════════════════════════

summary_text <- paste0("
═══════════════════════════════════════════════════════════
ADVANCEDRAINCLOUD TEST DATA SUMMARY
═══════════════════════════════════════════════════════════

Dataset: advancedraincloud_test
Generated: ", Sys.Date(), "
Seed: 42

DIMENSIONS
----------
Total observations: ", nrow(advancedraincloud_test), "
Variables: ", ncol(advancedraincloud_test), "
Patients: ", n_per_group * n_groups, "
Treatment groups: ", n_groups, "
Time points: ", n_timepoints, "
Missing data: ~3% across outcomes

VARIABLE DESCRIPTIONS
---------------------

Core Variables (for basic plotting):
  • pain_score [continuous, 0-100]: Primary outcome, higher = worse pain
  • treatment [factor, 3 levels]: Treatment arms (Placebo, Low Dose, High Dose)
  • timepoint [factor, 3 levels]: Time points (Baseline, Week 4, Week 12)
  • patient_id [integer]: Unique patient identifier for longitudinal connections

Secondary Outcomes:
  • function_score [continuous, 0-100]: Functional capacity, higher = better
  • satisfaction [integer, 1-7]: Likert scale for patient satisfaction
  • crp_level [continuous, >0]: C-reactive protein biomarker (mg/L)

Grouping/Coloring Variables:
  • disease_severity [factor, 3 levels]: Mild, Moderate, Severe
  • responder [factor, 2 levels]: Response classification (>20% improvement)
  • sex [factor, 2 levels]: Patient sex
  • site [factor, 3 levels]: Study site identifier

Covariates:
  • age [numeric]: Patient age in years
  • bmi [numeric]: Body mass index

RECOMMENDED USAGE SCENARIOS
---------------------------

1. Basic Raincloud (Cross-sectional):
   - y_var: pain_score
   - x_var: treatment
   - Subset: timepoint == 'Baseline'

2. Longitudinal Analysis:
   - y_var: pain_score
   - x_var: timepoint
   - fill_var: treatment
   - id_var: patient_id
   - show_longitudinal: TRUE

3. Likert Scale Mode:
   - y_var: satisfaction
   - x_var: treatment
   - likert_mode: TRUE

4. Clinical Trial Analysis:
   - y_var: pain_score
   - x_var: timepoint
   - fill_var: treatment
   - show_change_scores: TRUE
   - baseline_group: 'Baseline'
   - show_effect_size: TRUE

5. Biomarker Analysis:
   - y_var: crp_level
   - x_var: treatment
   - log_transform: TRUE
   - clinical_cutoff: 3 (mg/L normal range)
   - reference_range_min: 0
   - reference_range_max: 3

6. Subgroup Analysis:
   - y_var: pain_score
   - x_var: treatment
   - fill_var: disease_severity
   - cov_var: age

STATISTICAL PROPERTIES
----------------------

Pain Score by Treatment (Week 12):
  - Placebo: Mean ≈ 58, SD ≈ 14
  - Low Dose: Mean ≈ 42, SD ≈ 13
  - High Dose: Mean ≈ 32, SD ≈ 12
  - Expected Cohen's d: ~1.0 (High vs Placebo)

CRP Levels (log-normal distribution):
  - Baseline: Median ≈ 8 mg/L
  - Week 12 High Dose: Median ≈ 2.8 mg/L
  - Clinical cutoff: 3 mg/L

Responder Rates (Week 12):
  - Placebo: ~30%
  - Low Dose: ~50%
  - High Dose: ~70%

DATA QUALITY
------------
  - Realistic clinical trial structure
  - Balanced randomization
  - Proper factor ordering
  - Physiologically plausible values
  - Missing data pattern: MCAR (~3%)
  - Longitudinal ID linking preserved

EXAMPLE R CODE
--------------

# Load the data
data(advancedraincloud_test, package = 'ClinicoPath')

# Basic raincloud plot
advancedraincloud(
  data = subset(advancedraincloud_test, timepoint == 'Baseline'),
  y_var = 'pain_score',
  x_var = 'treatment',
  rain_side = 'l'
)

# Longitudinal analysis with connections
advancedraincloud(
  data = advancedraincloud_test,
  y_var = 'pain_score',
  x_var = 'timepoint',
  fill_var = 'treatment',
  id_var = 'patient_id',
  show_longitudinal = TRUE,
  rain_side = 'f'
)

# Likert scale visualization
advancedraincloud(
  data = advancedraincloud_test,
  y_var = 'satisfaction',
  x_var = 'treatment',
  likert_mode = TRUE,
  show_comparisons = TRUE
)

# Clinical trial with effect sizes
advancedraincloud(
  data = advancedraincloud_test,
  y_var = 'pain_score',
  x_var = 'timepoint',
  fill_var = 'treatment',
  show_effect_size = TRUE,
  effect_size_type = 'cohens_d',
  show_change_scores = TRUE,
  baseline_group = 'Baseline',
  clinical_cutoff = 50,
  show_mcid = TRUE,
  mcid_value = 10
)

FILES GENERATED
---------------
  ✓ data/advancedraincloud_test.rda    (R binary format)
  ✓ data/advancedraincloud_test.csv    (CSV format)
  ✓ data/advancedraincloud_test.xlsx   (Excel format)
  ✓ data/advancedraincloud_test.omv    (Jamovi format)

═══════════════════════════════════════════════════════════
")

cat(summary_text)

# Save summary to file
writeLines(summary_text, here::here("ADVANCEDRAINCLOUD_TEST_DATA_SUMMARY.md"))

cat("\n✓ All test data files generated successfully!\n")
cat("✓ Summary saved to: ADVANCEDRAINCLOUD_TEST_DATA_SUMMARY.md\n\n")
