# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjwithinstats
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the jjwithinstats jamovi function
# (within-subjects/repeated measures box-violin plots)
#
# Generated: 2026-01-05
# Seed: 42
# Observations: 80 subjects × 3-4 timepoints

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══════════════════════════════════════════════════════════
# 1. Three-Timepoint Clinical Trial Dataset
# ═══════════════════════════════════════════════════════════

n <- 80

# Generate baseline characteristics
jjwithinstats_test <- tibble(
  subject_id = 1:n,

  # Baseline tumor size (wide format data)
  baseline = rnorm(n, mean = 50, sd = 12),

  # Week 4 measurement (with realistic improvement)
  week4 = NA_real_,

  # Week 12 measurement (continued improvement)
  week12 = NA_real_
)

# Generate correlated within-subject measurements with treatment effect
for (i in 1:n) {
  base_value <- jjwithinstats_test$baseline[i]

  # Week 4: Individual response + overall treatment effect
  individual_response <- rnorm(1, mean = -8, sd = 5)  # Average reduction -8mm
  jjwithinstats_test$week4[i] <- base_value + individual_response

  # Week 12: Further improvement with correlation to Week 4
  week4_value <- jjwithinstats_test$week4[i]
  additional_change <- rnorm(1, mean = -5, sd = 4)  # Additional -5mm reduction
  jjwithinstats_test$week12[i] <- week4_value + additional_change
}

# Ensure no negative values (tumor size can't be negative)
jjwithinstats_test <- jjwithinstats_test %>%
  mutate(
    baseline = pmax(baseline, 5),
    week4 = pmax(week4, 3),
    week12 = pmax(week12, 2)
  )


# ═══════════════════════════════════════════════════════════
# 2. Four-Timepoint Biomarker Tracking Dataset
# ═══════════════════════════════════════════════════════════

n_bio <- 60

jjwithinstats_biomarker <- tibble(
  patient_id = 1:n_bio,

  # Baseline biomarker level (log-normal distribution)
  month0 = rlnorm(n_bio, meanlog = 4, sdlog = 0.8),
  month1 = NA_real_,
  month3 = NA_real_,
  month6 = NA_real_
)

# Generate correlated biomarker trajectory
for (i in 1:n_bio) {
  base_log <- log(jjwithinstats_biomarker$month0[i])

  # Month 1: Early response
  jjwithinstats_biomarker$month1[i] <- exp(base_log + rnorm(1, mean = -0.3, sd = 0.15))

  # Month 3: Continued decrease
  month1_log <- log(jjwithinstats_biomarker$month1[i])
  jjwithinstats_biomarker$month3[i] <- exp(month1_log + rnorm(1, mean = -0.25, sd = 0.12))

  # Month 6: Plateau or slight increase
  month3_log <- log(jjwithinstats_biomarker$month3[i])
  jjwithinstats_biomarker$month6[i] <- exp(month3_log + rnorm(1, mean = -0.1, sd = 0.1))
}


# ═══════════════════════════════════════════════════════════
# 3. Two-Timepoint Paired Comparison Dataset
# ═══════════════════════════════════════════════════════════

n_pair <- 100

jjwithinstats_paired <- tibble(
  subject_id = 1:n_pair,

  # Pre-treatment pain score (0-100, higher is worse)
  pre_treatment = rbeta(n_pair, shape1 = 5, shape2 = 3) * 100,

  # Post-treatment pain score
  post_treatment = NA_real_
)

# Generate post-treatment with individual response variability
for (i in 1:n_pair) {
  pre_value <- jjwithinstats_paired$pre_treatment[i]

  # Treatment reduces pain by 20-40 points on average
  reduction <- rnorm(1, mean = 30, sd = 12)
  jjwithinstats_paired$post_treatment[i] <- pmax(0, pre_value - reduction)
}


# ═══════════════════════════════════════════════════════════
# 4. Laboratory Values Dataset (with outliers)
# ═══════════════════════════════════════════════════════════

n_lab <- 70

jjwithinstats_laboratory <- tibble(
  patient_id = 1:n_lab,

  # Baseline lab value
  baseline_lab = rnorm(n_lab, mean = 100, sd = 20),
  week2_lab = NA_real_,
  week4_lab = NA_real_,
  week8_lab = NA_real_
)

# Generate lab values with treatment effect
for (i in 1:n_lab) {
  base_val <- jjwithinstats_laboratory$baseline_lab[i]

  # Week 2: Early changes
  jjwithinstats_laboratory$week2_lab[i] <- base_val + rnorm(1, mean = -8, sd = 6)

  # Week 4: Continued improvement
  week2_val <- jjwithinstats_laboratory$week2_lab[i]
  jjwithinstats_laboratory$week4_lab[i] <- week2_val + rnorm(1, mean = -6, sd = 5)

  # Week 8: Plateau
  week4_val <- jjwithinstats_laboratory$week4_lab[i]
  jjwithinstats_laboratory$week8_lab[i] <- week4_val + rnorm(1, mean = -2, sd = 4)
}

# Add outliers (~10%)
n_outliers <- round(n_lab * 0.10)
outlier_indices <- sample(n_lab, n_outliers)
jjwithinstats_laboratory$week4_lab[outlier_indices] <-
  jjwithinstats_laboratory$week4_lab[outlier_indices] + rnorm(n_outliers, mean = 40, sd = 15)


# ═══════════════════════════════════════════════════════════
# 5. Quality of Life Scores Dataset
# ═══════════════════════════════════════════════════════════

n_qol <- 90

jjwithinstats_qol <- tibble(
  subject_id = 1:n_qol,

  # Baseline QoL (0-100, higher is better)
  qol_baseline = rnorm(n_qol, mean = 45, sd = 12),
  qol_month1 = NA_real_,
  qol_month3 = NA_real_
)

# Generate QoL trajectory with improvement
for (i in 1:n_qol) {
  base_qol <- jjwithinstats_qol$qol_baseline[i]

  # Month 1: Moderate improvement
  jjwithinstats_qol$qol_month1[i] <- pmin(100, base_qol + rnorm(1, mean = 12, sd = 8))

  # Month 3: Further improvement
  month1_qol <- jjwithinstats_qol$qol_month1[i]
  jjwithinstats_qol$qol_month3[i] <- pmin(100, month1_qol + rnorm(1, mean = 8, sd = 6))
}

# Ensure valid range [0, 100]
jjwithinstats_qol <- jjwithinstats_qol %>%
  mutate(
    qol_baseline = pmax(0, pmin(100, qol_baseline)),
    qol_month1 = pmax(0, pmin(100, qol_month1)),
    qol_month3 = pmax(0, pmin(100, qol_month3))
  )


# ═══════════════════════════════════════════════════════════
# 6. Symptom Severity Dataset (Ordinal-like but continuous)
# ═══════════════════════════════════════════════════════════

n_symp <- 75

jjwithinstats_symptoms <- tibble(
  patient_id = 1:n_symp,

  # Symptom severity (1-10 scale, but continuous)
  symptom_pre = rnorm(n_symp, mean = 7.5, sd = 1.5),
  symptom_week4 = NA_real_,
  symptom_week8 = NA_real_,
  symptom_week12 = NA_real_
)

# Generate symptom reduction over time
for (i in 1:n_symp) {
  pre_symp <- jjwithinstats_symptoms$symptom_pre[i]

  # Week 4: First reduction
  jjwithinstats_symptoms$symptom_week4[i] <- pre_symp + rnorm(1, mean = -1.5, sd = 0.8)

  # Week 8: Continued reduction
  week4_symp <- jjwithinstats_symptoms$symptom_week4[i]
  jjwithinstats_symptoms$symptom_week8[i] <- week4_symp + rnorm(1, mean = -1.2, sd = 0.7)

  # Week 12: Final assessment
  week8_symp <- jjwithinstats_symptoms$symptom_week8[i]
  jjwithinstats_symptoms$symptom_week12[i] <- week8_symp + rnorm(1, mean = -0.8, sd = 0.6)
}

# Ensure valid range [1, 10]
jjwithinstats_symptoms <- jjwithinstats_symptoms %>%
  mutate(
    symptom_pre = pmax(1, pmin(10, symptom_pre)),
    symptom_week4 = pmax(1, pmin(10, symptom_week4)),
    symptom_week8 = pmax(1, pmin(10, symptom_week8)),
    symptom_week12 = pmax(1, pmin(10, symptom_week12))
  )


# ═══════════════════════════════════════════════════════════
# 7. Add Missing Data (~3%)
# ═══════════════════════════════════════════════════════════

# Add missing data to main dataset
n_missing <- round(n * 0.03)
jjwithinstats_test$week4[sample(n, n_missing)] <- NA
jjwithinstats_test$week12[sample(n, n_missing)] <- NA

# Add missing data to biomarker dataset
jjwithinstats_biomarker$month3[sample(n_bio, round(n_bio * 0.03))] <- NA


# ═══════════════════════════════════════════════════════════
# Save All Datasets
# ═══════════════════════════════════════════════════════════

# 1. Main comprehensive dataset
save(jjwithinstats_test, file = here("data", "jjwithinstats_test.rda"))
write.csv(jjwithinstats_test, file = here("data", "jjwithinstats_test.csv"), row.names = FALSE)
writexl::write_xlsx(
  list(
    comprehensive = jjwithinstats_test,
    biomarker = jjwithinstats_biomarker,
    paired = jjwithinstats_paired,
    laboratory = jjwithinstats_laboratory,
    qol = jjwithinstats_qol,
    symptoms = jjwithinstats_symptoms
  ),
  path = here("data", "jjwithinstats_test.xlsx")
)
jmvReadWrite::write_omv(jjwithinstats_test, here("data", "jjwithinstats_test.omv"))

# 2. Individual datasets
save(jjwithinstats_biomarker, file = here("data", "jjwithinstats_biomarker.rda"))
save(jjwithinstats_paired, file = here("data", "jjwithinstats_paired.rda"))
save(jjwithinstats_laboratory, file = here("data", "jjwithinstats_laboratory.rda"))
save(jjwithinstats_qol, file = here("data", "jjwithinstats_qol.rda"))
save(jjwithinstats_symptoms, file = here("data", "jjwithinstats_symptoms.rda"))


# ═══════════════════════════════════════════════════════════
# Generate Summary Documentation
# ═══════════════════════════════════════════════════════════

summary_text <- paste0("
═══════════════════════════════════════════════════════════
JJWITHINSTATS TEST DATA SUMMARY
═══════════════════════════════════════════════════════════

Dataset: jjwithinstats_test (Within-Subjects/Repeated Measures)
Generated: ", Sys.Date(), "
Seed: 42

DIMENSIONS
----------
Main Dataset (jjwithinstats_test):
  Subjects: ", n, "
  Timepoints: 3 (Baseline, Week 4, Week 12)
  Format: Wide (each timepoint is a column)
  Missing data: ~3% in week4 and week12

Biomarker Tracking (jjwithinstats_biomarker):
  Subjects: ", n_bio, "
  Timepoints: 4 (Month 0, 1, 3, 6)
  Distribution: Log-normal (right-skewed)
  Missing data: ~3% in month3

Paired Comparison (jjwithinstats_paired):
  Subjects: ", n_pair, "
  Timepoints: 2 (Pre, Post)
  Format: Before-after pain scores

Laboratory Values (jjwithinstats_laboratory):
  Subjects: ", n_lab, "
  Timepoints: 4 (Baseline, Week 2, 4, 8)
  Outliers: ~10% in week4_lab

Quality of Life (jjwithinstats_qol):
  Subjects: ", n_qol, "
  Timepoints: 3 (Baseline, Month 1, 3)
  Range: 0-100 (bounded)

Symptom Severity (jjwithinstats_symptoms):
  Subjects: ", n_symp, "
  Timepoints: 4 (Pre, Week 4, 8, 12)
  Range: 1-10 (ordinal-like continuous)

DATA STRUCTURE (WIDE FORMAT)
-----------------------------

Each dataset has one row per subject with multiple columns for timepoints:
  • subject_id/patient_id: Unique identifier
  • baseline, week4, week12: Repeated measurements (main dataset)
  • month0, month1, month3, month6: Biomarker timepoints
  • pre_treatment, post_treatment: Paired measurements

This is DIFFERENT from long format (one row per measurement).

VARIABLE DESCRIPTIONS
---------------------

Main Dataset (Tumor Size):
  • baseline [numeric, ≥5]: Baseline tumor size in mm
  • week4 [numeric, ≥3]: Week 4 tumor size
  • week12 [numeric, ≥2]: Week 12 tumor size

Biomarker Dataset:
  • month0 [numeric, >0]: Baseline biomarker (log-normal)
  • month1 [numeric]: Month 1 biomarker
  • month3 [numeric]: Month 3 biomarker
  • month6 [numeric]: Month 6 biomarker

Paired Dataset:
  • pre_treatment [numeric, 0-100]: Pre-treatment pain (VAS)
  • post_treatment [numeric, 0-100]: Post-treatment pain

Laboratory Dataset:
  • baseline_lab [numeric]: Baseline lab value
  • week2_lab [numeric]: Week 2 value
  • week4_lab [numeric]: Week 4 value (with outliers)
  • week8_lab [numeric]: Week 8 value

Quality of Life Dataset:
  • qol_baseline [numeric, 0-100]: Baseline QoL
  • qol_month1 [numeric, 0-100]: Month 1 QoL
  • qol_month3 [numeric, 0-100]: Month 3 QoL

Symptom Severity Dataset:
  • symptom_pre [numeric, 1-10]: Pre-treatment severity
  • symptom_week4 [numeric, 1-10]: Week 4 severity
  • symptom_week8 [numeric, 1-10]: Week 8 severity
  • symptom_week12 [numeric, 1-10]: Week 12 severity

STATISTICAL APPROACHES COVERED
-------------------------------

Parametric (typestatistics = 'parametric'):
  • Repeated measures ANOVA (3+ timepoints)
  • Paired t-test (2 timepoints)
  • Assumes normality of differences
  • Example: QoL scores, symptom severity

Nonparametric (typestatistics = 'nonparametric'):
  • Friedman test (3+ timepoints)
  • Wilcoxon signed-rank test (2 timepoints)
  • No distribution assumptions
  • Example: Biomarker data (log-normal)

Robust (typestatistics = 'robust'):
  • Trimmed means
  • Resistant to outliers
  • Example: Laboratory values with outliers

Bayesian (typestatistics = 'bayes'):
  • Bayesian repeated measures ANOVA
  • Bayesian paired t-test
  • Bayes Factor for evidence assessment
  • Example: Any continuous measurements

DATA CHARACTERISTICS
--------------------

Treatment Effects Over Time:
  • Tumor size: Baseline (50mm) → Week 4 (42mm) → Week 12 (37mm)
  • Biomarker: Progressive decrease over 6 months (log-normal)
  • Pain: Pre (62) → Post (32), mean reduction = 30 points
  • Lab values: Gradual improvement with plateau at Week 8
  • QoL: Baseline (45) → Month 1 (57) → Month 3 (65)
  • Symptoms: Steady decline from 7.5 to 4.2 over 12 weeks

Within-Subject Correlations:
  • Realistic correlation between consecutive measurements
  • Individual response variability preserved
  • Some subjects improve more than others
  • Maintains subject-specific trajectories

Biomarker Dataset (Log-Normal):
  • Right-skewed distribution
  • Demonstrates need for nonparametric tests
  • Realistic biomarker trajectory

Laboratory Dataset (With Outliers):
  • 10% contamination at Week 4
  • Tests robust statistics
  • Realistic lab measurement variability

Paired Dataset:
  • Clear pre-post difference
  • Large effect size (Cohen's d ≈ 2.0)
  • Suitable for paired t-test demonstration

CLINICAL PRESET SCENARIOS
--------------------------

1. Biomarker Tracking (clinicalpreset = 'biomarker'):
   Use: jjwithinstats_biomarker
   dep1 = month0, dep2 = month1, dep3 = month3, dep4 = month6
   typestatistics = 'nonparametric'  # For log-normal data
   → Friedman test for non-normal biomarker trajectory

2. Treatment Response (clinicalpreset = 'treatment'):
   Use: jjwithinstats_test or jjwithinstats_qol
   dep1 = baseline, dep2 = week4, dep3 = week12
   typestatistics = 'parametric'  # For normally distributed outcomes
   → Repeated measures ANOVA for treatment efficacy

3. Laboratory Values (clinicalpreset = 'laboratory'):
   Use: jjwithinstats_laboratory
   dep1 = baseline_lab, dep2 = week2_lab, dep3 = week4_lab, dep4 = week8_lab
   typestatistics = 'robust'  # For outlier-contaminated data
   → Robust repeated measures for lab monitoring

EXAMPLE R CODE
--------------

# Load test data
data(jjwithinstats_test, package = 'ClinicoPath')

# 1. Basic three-timepoint analysis
jjwithinstats(
  data = jjwithinstats_test,
  dep1 = 'baseline',
  dep2 = 'week4',
  dep3 = 'week12',
  typestatistics = 'parametric'
)

# 2. With pairwise comparisons and trajectories
jjwithinstats(
  data = jjwithinstats_test,
  dep1 = 'baseline',
  dep2 = 'week4',
  dep3 = 'week12',
  pairwisecomparisons = TRUE,
  padjustmethod = 'bonferroni',
  pointpath = TRUE,  # Show individual trajectories
  centralityplotting = TRUE,
  centralitytype = 'parametric'
)

# 3. Two-timepoint paired analysis
data(jjwithinstats_paired)
jjwithinstats(
  data = jjwithinstats_paired,
  dep1 = 'pre_treatment',
  dep2 = 'post_treatment',
  typestatistics = 'parametric',
  pointpath = TRUE,
  mytitle = 'Pain Reduction: Pre vs Post Treatment'
)

# 4. Biomarker tracking (nonparametric)
data(jjwithinstats_biomarker)
jjwithinstats(
  data = jjwithinstats_biomarker,
  dep1 = 'month0',
  dep2 = 'month1',
  dep3 = 'month3',
  dep4 = 'month6',
  clinicalpreset = 'biomarker',
  typestatistics = 'nonparametric',
  centralityplotting = TRUE,
  centralitytype = 'nonparametric'
)

# 5. Laboratory values with outliers (robust)
data(jjwithinstats_laboratory)
jjwithinstats(
  data = jjwithinstats_laboratory,
  dep1 = 'baseline_lab',
  dep2 = 'week2_lab',
  dep3 = 'week4_lab',
  dep4 = 'week8_lab',
  clinicalpreset = 'laboratory',
  typestatistics = 'robust',
  centralityplotting = TRUE,
  centralitytype = 'robust'
)

# 6. Bayesian analysis
jjwithinstats(
  data = jjwithinstats_test,
  dep1 = 'baseline',
  dep2 = 'week4',
  dep3 = 'week12',
  typestatistics = 'bayes',
  bfmessage = TRUE,
  centralityplotting = TRUE
)

# 7. Quality of life trajectory
data(jjwithinstats_qol)
jjwithinstats(
  data = jjwithinstats_qol,
  dep1 = 'qol_baseline',
  dep2 = 'qol_month1',
  dep3 = 'qol_month3',
  clinicalpreset = 'treatment',
  pairwisecomparisons = TRUE,
  pointpath = TRUE,
  centralitypath = TRUE,
  mytitle = 'Quality of Life Improvement Over Treatment'
)

# 8. Symptom severity over time
data(jjwithinstats_symptoms)
jjwithinstats(
  data = jjwithinstats_symptoms,
  dep1 = 'symptom_pre',
  dep2 = 'symptom_week4',
  dep3 = 'symptom_week8',
  dep4 = 'symptom_week12',
  typestatistics = 'parametric',
  pairwisecomparisons = TRUE,
  pointpath = TRUE,
  violin = TRUE,
  boxplot = TRUE,
  point = TRUE
)

# 9. Publication-ready with ggpubr
jjwithinstats(
  data = jjwithinstats_test,
  dep1 = 'baseline',
  dep2 = 'week4',
  dep3 = 'week12',
  addGGPubrPlot = TRUE,
  ggpubrPlotType = 'paired',  # Shows individual lines
  ggpubrPalette = 'jco',
  ggpubrShowLines = TRUE,
  ggpubrAddStats = TRUE
)

# 10. With custom aesthetics
jjwithinstats(
  data = jjwithinstats_test,
  dep1 = 'baseline',
  dep2 = 'week4',
  dep3 = 'week12',
  mytitle = 'Tumor Response Over 12 Weeks',
  xtitle = 'Assessment Time',
  ytitle = 'Tumor Size (mm)',
  centralityplotting = TRUE,
  resultssubtitle = TRUE,
  k = 2,
  conflevel = 0.95
)

FILES GENERATED
---------------
  ✓ data/jjwithinstats_test.rda           (Comprehensive dataset)
  ✓ data/jjwithinstats_biomarker.rda      (4 timepoints, log-normal)
  ✓ data/jjwithinstats_paired.rda         (2 timepoints, pre-post)
  ✓ data/jjwithinstats_laboratory.rda     (4 timepoints, with outliers)
  ✓ data/jjwithinstats_qol.rda            (3 timepoints, QoL)
  ✓ data/jjwithinstats_symptoms.rda       (4 timepoints, symptoms)
  ✓ data/jjwithinstats_test.csv           (CSV format)
  ✓ data/jjwithinstats_test.xlsx          (Excel with multiple sheets)
  ✓ data/jjwithinstats_test.omv           (Jamovi format)

═══════════════════════════════════════════════════════════

✓ All test data files generated successfully!
✓ Summary saved to: JJWITHINSTATS_TEST_DATA_SUMMARY.md
")

cat(summary_text)
writeLines(summary_text, here("JJWITHINSTATS_TEST_DATA_SUMMARY.md"))

cat("\n✓ jjwithinstats test data generation complete!\n")
