# ═══════════════════════════════════════════════════════════
# Test Data Generation: statsplot2
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the statsplot2 jamovi function
#
# Generated: 2026-01-04
# Seed: 42
# Scenarios:
#   1. Continuous outcome vs categorical groups (violin/box plots)
#   2. Categorical outcome vs categorical groups (bar charts)
#   3. Continuous vs continuous (scatter plots)
#   4. Repeated measures data (within-subjects)
#   5. Data with split-by variable (grouped plots)
#   6. Various statistical distributions (normal, skewed, outliers)
#
# Function purpose: Automatic plot selection based on variable data types
# Supports: Independent and repeated measures designs with multiple plot types

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══════════════════════════════════════════════════════════
# 1. CLINICAL TRIAL DATA (Continuous vs Categorical)
# ═══════════════════════════════════════════════════════════
# Most common scenario: Compare continuous outcomes across treatment groups

n_per_group <- 60
n_groups <- 3

clinical_trial_data <- tibble(
  # ─────────────────────────────────────────────────────────
  # Patient identifiers
  # ─────────────────────────────────────────────────────────
  patient_id = 1:(n_per_group * n_groups),

  # ─────────────────────────────────────────────────────────
  # Treatment groups (categorical x-axis)
  # ─────────────────────────────────────────────────────────
  treatment = rep(c("Placebo", "Low Dose", "High Dose"), each = n_per_group),

  # ─────────────────────────────────────────────────────────
  # Continuous outcomes (y-axis)
  # ─────────────────────────────────────────────────────────
  # Tumor size reduction (mm) - normally distributed
  tumor_reduction = c(
    rnorm(n_per_group, mean = 5, sd = 8),    # Placebo: small reduction
    rnorm(n_per_group, mean = 15, sd = 10),  # Low Dose: moderate reduction
    rnorm(n_per_group, mean = 25, sd = 12)   # High Dose: large reduction
  ),

  # Pain score (0-100 VAS) - slightly skewed
  pain_score = c(
    rbeta(n_per_group, 6, 3) * 100,  # Placebo: higher pain
    rbeta(n_per_group, 4, 5) * 100,  # Low Dose: moderate pain
    rbeta(n_per_group, 2, 6) * 100   # High Dose: lower pain
  ),

  # Quality of life score (0-100) - normal distribution
  qol_score = c(
    rnorm(n_per_group, mean = 55, sd = 15),
    rnorm(n_per_group, mean = 70, sd = 12),
    rnorm(n_per_group, mean = 80, sd = 10)
  ),

  # ─────────────────────────────────────────────────────────
  # Categorical outcomes
  # ─────────────────────────────────────────────────────────
  response_status = c(
    sample(c("No Response", "Partial Response", "Complete Response"),
           n_per_group, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    sample(c("No Response", "Partial Response", "Complete Response"),
           n_per_group, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    sample(c("No Response", "Partial Response", "Complete Response"),
           n_per_group, replace = TRUE, prob = c(0.15, 0.35, 0.5))
  ),

  # ─────────────────────────────────────────────────────────
  # Split-by variables (for grouped plots)
  # ─────────────────────────────────────────────────────────
  tumor_stage = rep(
    sample(c("Stage I-II", "Stage III-IV"), n_per_group * n_groups,
           replace = TRUE, prob = c(0.4, 0.6))
  ),

  sex = rep(
    sample(c("Male", "Female"), n_per_group * n_groups,
           replace = TRUE, prob = c(0.55, 0.45))
  ),

  age_group = rep(
    sample(c("<50", "50-65", ">65"), n_per_group * n_groups,
           replace = TRUE, prob = c(0.2, 0.5, 0.3))
  ),

  # ─────────────────────────────────────────────────────────
  # Continuous covariates
  # ─────────────────────────────────────────────────────────
  age = round(rnorm(n_per_group * n_groups, mean = 62, sd = 12)),
  bmi = round(rnorm(n_per_group * n_groups, mean = 27, sd = 5), 1),

  # Biomarker level (for continuous vs continuous plots)
  biomarker_level = rnorm(n_per_group * n_groups, mean = 50, sd = 20)
)

# Add correlation between biomarker and tumor reduction
clinical_trial_data$tumor_reduction <- clinical_trial_data$tumor_reduction +
  0.3 * (clinical_trial_data$biomarker_level - 50)

# Constrain values to realistic ranges
clinical_trial_data <- clinical_trial_data %>%
  mutate(
    tumor_reduction = pmax(0, tumor_reduction),
    pain_score = pmax(0, pmin(100, pain_score)),
    qol_score = pmax(0, pmin(100, qol_score)),
    biomarker_level = pmax(0, biomarker_level)
  )

# Convert to factors
clinical_trial_data <- clinical_trial_data %>%
  mutate(
    treatment = factor(treatment, levels = c("Placebo", "Low Dose", "High Dose")),
    response_status = factor(response_status,
                             levels = c("No Response", "Partial Response", "Complete Response")),
    tumor_stage = factor(tumor_stage),
    sex = factor(sex),
    age_group = factor(age_group, levels = c("<50", "50-65", ">65"))
  )

# ═══════════════════════════════════════════════════════════
# 2. REPEATED MEASURES DATA (Within-Subjects)
# ═══════════════════════════════════════════════════════════
# Longitudinal data: same patients measured at multiple timepoints

n_patients <- 80

repeated_measures_data <- tibble(
  # ─────────────────────────────────────────────────────────
  # Patient identifiers
  # ─────────────────────────────────────────────────────────
  patient_id = rep(1:n_patients, each = 3),

  # ─────────────────────────────────────────────────────────
  # Timepoints (within-subjects factor)
  # ─────────────────────────────────────────────────────────
  timepoint = rep(c("Baseline", "Week 4", "Week 12"), times = n_patients),

  # ─────────────────────────────────────────────────────────
  # Continuous repeated measures
  # ─────────────────────────────────────────────────────────
  symptom_severity = NA_real_,

  # ─────────────────────────────────────────────────────────
  # Categorical repeated measures (for alluvial plots)
  # ─────────────────────────────────────────────────────────
  disease_status = NA_character_,

  # ─────────────────────────────────────────────────────────
  # Treatment arm (between-subjects factor)
  # ─────────────────────────────────────────────────────────
  treatment_arm = rep(
    sample(c("Treatment A", "Treatment B"), n_patients, replace = TRUE),
    each = 3
  )
)

# Generate realistic repeated measures with individual trajectories
for (i in 1:n_patients) {
  baseline_severity <- rnorm(1, mean = 70, sd = 15)
  improvement_rate <- rnorm(1, mean = 0.4, sd = 0.1)

  idx <- which(repeated_measures_data$patient_id == i)

  repeated_measures_data$symptom_severity[idx[1]] <- baseline_severity
  repeated_measures_data$symptom_severity[idx[2]] <- baseline_severity * (1 - improvement_rate * 0.5) + rnorm(1, 0, 5)
  repeated_measures_data$symptom_severity[idx[3]] <- baseline_severity * (1 - improvement_rate) + rnorm(1, 0, 8)

  # Disease status transitions (Markov-like)
  baseline_status <- sample(c("Active", "Moderate", "Mild"), 1, prob = c(0.5, 0.3, 0.2))

  if (baseline_status == "Active") {
    week4 <- sample(c("Active", "Moderate", "Mild"), 1, prob = c(0.4, 0.4, 0.2))
  } else if (baseline_status == "Moderate") {
    week4 <- sample(c("Active", "Moderate", "Mild"), 1, prob = c(0.2, 0.4, 0.4))
  } else {
    week4 <- sample(c("Moderate", "Mild", "Remission"), 1, prob = c(0.2, 0.5, 0.3))
  }

  if (week4 == "Active") {
    week12 <- sample(c("Active", "Moderate", "Mild"), 1, prob = c(0.3, 0.5, 0.2))
  } else if (week4 == "Moderate") {
    week12 <- sample(c("Moderate", "Mild", "Remission"), 1, prob = c(0.3, 0.4, 0.3))
  } else if (week4 == "Mild") {
    week12 <- sample(c("Mild", "Remission"), 1, prob = c(0.4, 0.6))
  } else {
    week12 <- "Remission"
  }

  repeated_measures_data$disease_status[idx[1]] <- baseline_status
  repeated_measures_data$disease_status[idx[2]] <- week4
  repeated_measures_data$disease_status[idx[3]] <- week12
}

# Constrain continuous values
repeated_measures_data$symptom_severity <- pmax(0, pmin(100, repeated_measures_data$symptom_severity))

# Convert to factors
repeated_measures_data <- repeated_measures_data %>%
  mutate(
    timepoint = factor(timepoint, levels = c("Baseline", "Week 4", "Week 12")),
    disease_status = factor(disease_status,
                           levels = c("Active", "Moderate", "Mild", "Remission")),
    treatment_arm = factor(treatment_arm)
  )

# ═══════════════════════════════════════════════════════════
# 3. COMPREHENSIVE TEST DATASET
# ═══════════════════════════════════════════════════════════
# Combines multiple scenario types for comprehensive testing

statsplot2_test <- clinical_trial_data %>%
  select(
    # Core variables for plotting
    patient_id,

    # Continuous outcomes
    tumor_reduction,
    pain_score,
    qol_score,
    biomarker_level,
    age,
    bmi,

    # Categorical outcomes
    response_status,

    # Grouping variables
    treatment,
    tumor_stage,
    sex,
    age_group
  )

# Add some missing data (~3%)
n_missing <- round(nrow(statsplot2_test) * 0.03)
statsplot2_test$tumor_reduction[sample(1:nrow(statsplot2_test), n_missing)] <- NA
statsplot2_test$pain_score[sample(1:nrow(statsplot2_test), n_missing)] <- NA
statsplot2_test$biomarker_level[sample(1:nrow(statsplot2_test), n_missing)] <- NA

# ═══════════════════════════════════════════════════════════
# 4. SPECIAL CASE DATASETS
# ═══════════════════════════════════════════════════════════

# Dataset with outliers (for robust statistics testing)
statsplot2_outliers <- statsplot2_test
outlier_indices <- sample(1:nrow(statsplot2_outliers), 10)
statsplot2_outliers$tumor_reduction[outlier_indices] <-
  statsplot2_outliers$tumor_reduction[outlier_indices] * 3

# Dataset with highly skewed data (for nonparametric testing)
statsplot2_skewed <- statsplot2_test %>%
  mutate(
    skewed_outcome = rexp(n(), rate = 0.1),
    log_transformed = log(skewed_outcome + 1)
  )

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# 1. RDA format (native R)
save(statsplot2_test, file = here::here("data", "statsplot2_test.rda"))
save(clinical_trial_data, file = here::here("data", "statsplot2_clinical.rda"))
save(repeated_measures_data, file = here::here("data", "statsplot2_repeated.rda"))
save(statsplot2_outliers, file = here::here("data", "statsplot2_outliers.rda"))
save(statsplot2_skewed, file = here::here("data", "statsplot2_skewed.rda"))

# 2. CSV format
write.csv(statsplot2_test,
          file = here::here("data", "statsplot2_test.csv"),
          row.names = FALSE)
write.csv(clinical_trial_data,
          file = here::here("data", "statsplot2_clinical.csv"),
          row.names = FALSE)
write.csv(repeated_measures_data,
          file = here::here("data", "statsplot2_repeated.csv"),
          row.names = FALSE)

# 3. Excel format (multiple sheets)
writexl::write_xlsx(
  list(
    comprehensive = statsplot2_test,
    clinical_trial = clinical_trial_data,
    repeated_measures = repeated_measures_data,
    outliers = statsplot2_outliers,
    skewed = statsplot2_skewed
  ),
  path = here::here("data", "statsplot2_test.xlsx")
)

# 4. Jamovi format (OMV)
jmvReadWrite::write_omv(statsplot2_test,
                        here::here("data", "statsplot2_test.omv"))

# ═══════════════════════════════════════════════════════════
# Generate Data Summary Report
# ═══════════════════════════════════════════════════════════

summary_text <- paste0("
═══════════════════════════════════════════════════════════
STATSPLOT2 TEST DATA SUMMARY
═══════════════════════════════════════════════════════════

Dataset: statsplot2_test (Automatic Plot Selection Data)
Generated: ", Sys.Date(), "
Seed: 42

DIMENSIONS
----------
Main Dataset (statsplot2_test):
  Observations: ", nrow(statsplot2_test), "
  Variables: ", ncol(statsplot2_test), "
  Treatment groups: 3 (Placebo, Low/High Dose)
  Missing data: ~3% across continuous outcomes

Clinical Trial Data (statsplot2_clinical):
  Observations: ", nrow(clinical_trial_data), "
  Variables: ", ncol(clinical_trial_data), "

Repeated Measures Data (statsplot2_repeated):
  Observations: ", nrow(repeated_measures_data), "
  Patients: ", length(unique(repeated_measures_data$patient_id)), "
  Timepoints: 3 (Baseline, Week 4, Week 12)

VARIABLE DESCRIPTIONS
---------------------

Continuous Outcomes (for dep parameter):
  • tumor_reduction [numeric, ≥0]: Tumor size reduction in mm
  • pain_score [numeric, 0-100]: Visual analog pain scale
  • qol_score [numeric, 0-100]: Quality of life score
  • biomarker_level [numeric, ≥0]: Biomarker concentration
  • age [integer]: Patient age in years
  • bmi [numeric]: Body mass index

Categorical Outcomes (for dep parameter):
  • response_status [factor, 3 levels]: No Response, Partial Response, Complete Response
  • disease_status [factor, 4 levels]: Active, Moderate, Mild, Remission (repeated data)

Grouping Variables (for group parameter):
  • treatment [factor, 3 levels]: Placebo, Low Dose, High Dose
  • tumor_stage [factor, 2 levels]: Stage I-II, Stage III-IV
  • sex [factor, 2 levels]: Male, Female
  • age_group [factor, 3 levels]: <50, 50-65, >65
  • timepoint [factor, 3 levels]: Baseline, Week 4, Week 12 (repeated data)
  • treatment_arm [factor, 2 levels]: Treatment A, Treatment B (repeated data)

Split-By Variables (for grvar parameter):
  • Any of the categorical grouping variables above

AUTOMATIC PLOT SELECTION SCENARIOS
-----------------------------------

1. Continuous vs Categorical (Violin/Box Plots):
   dep = tumor_reduction (continuous)
   group = treatment (categorical)
   → Generates violin plot with statistical comparisons

2. Categorical vs Categorical (Bar Charts):
   dep = response_status (categorical)
   group = treatment (categorical)
   → Generates grouped bar chart with proportions

3. Continuous vs Continuous (Scatter Plot):
   dep = tumor_reduction (continuous)
   group = biomarker_level (continuous)
   → Generates scatter plot with correlation

4. Repeated Measures - Continuous (Line Plot):
   dep = symptom_severity (continuous)
   group = timepoint (categorical)
   direction = repeated
   → Generates repeated measures plot with individual trajectories

5. Repeated Measures - Categorical (Alluvial):
   dep = disease_status (categorical, Baseline)
   group = disease_status (categorical, Week 12)
   direction = repeated
   → Generates alluvial flow diagram

6. Grouped/Faceted Plots:
   dep = tumor_reduction
   group = treatment
   grvar = tumor_stage
   → Generates plots split by tumor stage

STATISTICAL APPROACHES COVERED
-------------------------------

Parametric (distribution = 'p'):
  • Normally distributed continuous data
  • Uses t-tests, ANOVA
  • Example: qol_score (approximately normal)

Nonparametric (distribution = 'np'):
  • Skewed continuous data
  • Uses Mann-Whitney, Kruskal-Wallis
  • Example: pain_score (beta distribution)

Robust (distribution = 'r'):
  • Data with outliers
  • Uses trimmed means
  • Example: statsplot2_outliers dataset

Bayesian (distribution = 'bf'):
  • Provides Bayes Factor evidence
  • All variable types
  • Example: Any continuous outcome

DATA CHARACTERISTICS
--------------------

Clinical Trial Data:
  • Treatment effects: Placebo < Low Dose < High Dose
  • Tumor reduction: Mean difference ~20mm between Placebo and High Dose
  • Pain scores: Beta distributed (realistic VAS distribution)
  • Response rates: Increase with dose (10% → 50% complete response)
  • Correlation: Biomarker positively correlated with tumor reduction

Repeated Measures Data:
  • Individual patient trajectories preserved
  • Realistic improvement over time
  • Disease status transitions follow Markov-like patterns
  • Within-subject correlation structure maintained

Special Datasets:
  • Outliers: Contains ~10 extreme values for robust testing
  • Skewed: Exponentially distributed for nonparametric testing

EXAMPLE R CODE
--------------

# Load test data
data(statsplot2_test, package = 'ClinicoPath')

# 1. Continuous vs Categorical (automatic violin plot)
statsplot2(
  data = statsplot2_test,
  dep = 'tumor_reduction',
  group = 'treatment',
  direction = 'independent',
  distribution = 'p'
)

# 2. With split-by variable (grouped plots)
statsplot2(
  data = statsplot2_test,
  dep = 'pain_score',
  group = 'treatment',
  grvar = 'sex',
  distribution = 'np'
)

# 3. Continuous vs continuous (scatter plot)
statsplot2(
  data = statsplot2_test,
  dep = 'tumor_reduction',
  group = 'biomarker_level',
  direction = 'independent'
)

# 4. Categorical vs categorical (bar chart)
statsplot2(
  data = statsplot2_test,
  dep = 'response_status',
  group = 'treatment'
)

# 5. Repeated measures - continuous
data(statsplot2_repeated)
statsplot2(
  data = statsplot2_repeated,
  dep = 'symptom_severity',
  group = 'timepoint',
  direction = 'repeated',
  distribution = 'p'
)

# 6. Repeated measures - categorical (alluvial)
# Reshape data for alluvial plot
baseline_status <- subset(statsplot2_repeated,
                          timepoint == 'Baseline')$disease_status
week12_status <- subset(statsplot2_repeated,
                        timepoint == 'Week 12')$disease_status
alluvial_data <- data.frame(
  baseline = baseline_status,
  week12 = week12_status
)
statsplot2(
  data = alluvial_data,
  dep = 'baseline',
  group = 'week12',
  direction = 'repeated',
  alluvsty = 't1'
)

# 7. Robust statistics with outliers
data(statsplot2_outliers)
statsplot2(
  data = statsplot2_outliers,
  dep = 'tumor_reduction',
  group = 'treatment',
  distribution = 'r'
)

# 8. Bayesian analysis
statsplot2(
  data = statsplot2_test,
  dep = 'qol_score',
  group = 'treatment',
  distribution = 'bf'
)

FILES GENERATED
---------------
  ✓ data/statsplot2_test.rda           (Comprehensive dataset)
  ✓ data/statsplot2_clinical.rda       (Clinical trial data)
  ✓ data/statsplot2_repeated.rda       (Repeated measures)
  ✓ data/statsplot2_outliers.rda       (With outliers)
  ✓ data/statsplot2_skewed.rda         (Skewed distribution)
  ✓ data/statsplot2_test.csv           (CSV format)
  ✓ data/statsplot2_test.xlsx          (Excel with multiple sheets)
  ✓ data/statsplot2_test.omv           (Jamovi format)

═══════════════════════════════════════════════════════════
")

cat(summary_text)

# Save summary to file
writeLines(summary_text, here::here("STATSPLOT2_TEST_DATA_SUMMARY.md"))

cat("\n✓ All test data files generated successfully!\n")
cat("✓ Summary saved to: STATSPLOT2_TEST_DATA_SUMMARY.md\n\n")
