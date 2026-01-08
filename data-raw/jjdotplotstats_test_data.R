# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjdotplotstats
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the jjdotplotstats jamovi function
# which creates dot-style comparisons of continuous variables between groups.
#
# Generated: 2026-01-05
# Seed: 42
# Observations: Various (60-200 per dataset)

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# ═══════════════════════════════════════════════════════════
# Dataset 1: Clinical Response (jjdotplotstats_test)
# ═══════════════════════════════════════════════════════════
# Three treatment groups with clear differences

n_test <- 120

jjdotplotstats_test <- tibble(
  patient_id = 1:n_test,

  # Tumor reduction percentage (0-100)
  # Control: 20% (SD=15), Treatment A: 45% (SD=18), Treatment B: 65% (SD=20)
  tumor_reduction = c(
    rnorm(40, mean = 20, sd = 15),
    rnorm(40, mean = 45, sd = 18),
    rnorm(40, mean = 65, sd = 20)
  ),

  # Treatment group
  treatment = factor(rep(c("Control", "Treatment A", "Treatment B"), each = 40),
                     levels = c("Control", "Treatment A", "Treatment B")),

  # Hospital site for grouped analysis
  hospital = factor(sample(c("Site A", "Site B", "Site C"), n_test, replace = TRUE)),

  # Tumor stage for stratification
  tumor_stage = factor(sample(c("Early", "Advanced"), n_test, replace = TRUE))
)

# Ensure reasonable bounds (0-100%)
jjdotplotstats_test$tumor_reduction <- pmin(100, pmax(0, jjdotplotstats_test$tumor_reduction))

# Add ~3% missing data
n_missing_test <- round(n_test * 0.03)
jjdotplotstats_test$tumor_reduction[sample(n_test, n_missing_test)] <- NA


# ═══════════════════════════════════════════════════════════
# Dataset 2: Two-Group Comparison (jjdotplotstats_twogroup)
# ═══════════════════════════════════════════════════════════
# Pre-post or treatment vs control

n_two <- 80

jjdotplotstats_twogroup <- tibble(
  subject_id = 1:n_two,

  # Pain score (0-10 VAS)
  # Pre-treatment: 7.5 (SD=2), Post-treatment: 3.5 (SD=1.8)
  pain_score = c(
    rnorm(40, mean = 7.5, sd = 2.0),
    rnorm(40, mean = 3.5, sd = 1.8)
  ),

  # Timepoint
  timepoint = factor(rep(c("Pre-treatment", "Post-treatment"), each = 40),
                     levels = c("Pre-treatment", "Post-treatment")),

  # Gender for grouped analysis
  gender = factor(sample(c("Male", "Female"), n_two, replace = TRUE))
)

# Ensure reasonable bounds (0-10)
jjdotplotstats_twogroup$pain_score <- pmin(10, pmax(0, jjdotplotstats_twogroup$pain_score))


# ═══════════════════════════════════════════════════════════
# Dataset 3: Four-Group Comparison (jjdotplotstats_fourgroup)
# ═══════════════════════════════════════════════════════════
# Multiple treatment arms

n_four <- 160

jjdotplotstats_fourgroup <- tibble(
  patient_id = 1:n_four,

  # Efficacy score (0-100)
  # Placebo: 35, Low dose: 50, Medium dose: 65, High dose: 75
  efficacy_score = c(
    rnorm(40, mean = 35, sd = 12),
    rnorm(40, mean = 50, sd = 14),
    rnorm(40, mean = 65, sd = 13),
    rnorm(40, mean = 75, sd = 15)
  ),

  # Dose group
  dose = factor(rep(c("Placebo", "Low", "Medium", "High"), each = 40),
                levels = c("Placebo", "Low", "Medium", "High")),

  # Age group for stratification
  age_group = factor(sample(c("Young", "Middle", "Elderly"), n_four, replace = TRUE))
)

# Ensure reasonable bounds
jjdotplotstats_fourgroup$efficacy_score <- pmin(100, pmax(0, jjdotplotstats_fourgroup$efficacy_score))


# ═══════════════════════════════════════════════════════════
# Dataset 4: Skewed Data (jjdotplotstats_skewed)
# ═══════════════════════════════════════════════════════════
# Non-normal distributions requiring nonparametric tests

n_skew <- 90

# Generate log-normal (right-skewed) biomarker data
jjdotplotstats_skewed <- tibble(
  patient_id = 1:n_skew,

  # Biomarker (log-normal distribution)
  # Control: median ~8, Treatment: median ~4 (reduction)
  biomarker_level = c(
    exp(rnorm(45, mean = log(8), sd = 0.6)),
    exp(rnorm(45, mean = log(4), sd = 0.5))
  ),

  # Treatment group
  treatment = factor(rep(c("Control", "Active"), each = 45),
                     levels = c("Control", "Active")),

  # Disease type
  disease = factor(sample(c("Type 1", "Type 2"), n_skew, replace = TRUE))
)


# ═══════════════════════════════════════════════════════════
# Dataset 5: Outlier-Contaminated (jjdotplotstats_outliers)
# ═══════════════════════════════════════════════════════════
# Data with extreme values for robust statistics

n_out <- 100

jjdotplotstats_outliers <- tibble(
  subject_id = 1:n_out,

  # Response variable with outliers
  response = c(
    rnorm(50, mean = 50, sd = 10),
    rnorm(50, mean = 70, sd = 12)
  ),

  # Group
  group = factor(rep(c("Group A", "Group B"), each = 50),
                 levels = c("Group A", "Group B")),

  # Center for grouped analysis
  center = factor(sample(c("Center 1", "Center 2"), n_out, replace = TRUE))
)

# Add 10% outliers to each group
n_outliers_per_group <- 5
# Group A outliers (very high)
outlier_idx_a <- sample(1:50, n_outliers_per_group)
jjdotplotstats_outliers$response[outlier_idx_a] <-
  jjdotplotstats_outliers$response[outlier_idx_a] + rnorm(n_outliers_per_group, mean = 50, sd = 10)
# Group B outliers (very low)
outlier_idx_b <- sample(51:100, n_outliers_per_group)
jjdotplotstats_outliers$response[outlier_idx_b] <-
  jjdotplotstats_outliers$response[outlier_idx_b] - rnorm(n_outliers_per_group, mean = 40, sd = 8)


# ═══════════════════════════════════════════════════════════
# Dataset 6: Reference Value Testing (jjdotplotstats_reference)
# ═══════════════════════════════════════════════════════════
# For testing against a clinically meaningful threshold

n_ref <- 150

jjdotplotstats_reference <- tibble(
  patient_id = 1:n_ref,

  # Blood pressure reduction (mmHg)
  # Target reduction: ≥10 mmHg
  # Drug A: mean=8 (below target), Drug B: mean=15 (above target), Drug C: mean=12 (above target)
  bp_reduction = c(
    rnorm(50, mean = 8, sd = 5),
    rnorm(50, mean = 15, sd = 6),
    rnorm(50, mean = 12, sd = 5)
  ),

  # Drug
  drug = factor(rep(c("Drug A", "Drug B", "Drug C"), each = 50),
                levels = c("Drug A", "Drug B", "Drug C")),

  # Baseline severity
  severity = factor(sample(c("Mild", "Moderate", "Severe"), n_ref, replace = TRUE))
)

# Add reference threshold as attribute (10 mmHg)
attr(jjdotplotstats_reference, "reference_value") <- 10


# ═══════════════════════════════════════════════════════════
# Dataset 7: Quality of Life (jjdotplotstats_qol)
# ═══════════════════════════════════════════════════════════
# Bounded 0-100 scale with moderate differences

n_qol <- 120

jjdotplotstats_qol <- tibble(
  patient_id = 1:n_qol,

  # QoL score (0-100)
  # Standard care: 55, Intervention A: 65, Intervention B: 72
  qol_score = c(
    rnorm(40, mean = 55, sd = 18),
    rnorm(40, mean = 65, sd = 16),
    rnorm(40, mean = 72, sd = 15)
  ),

  # Intervention
  intervention = factor(rep(c("Standard Care", "Intervention A", "Intervention B"), each = 40),
                        levels = c("Standard Care", "Intervention A", "Intervention B")),

  # Comorbidity
  comorbidity = factor(sample(c("None", "Present"), n_qol, replace = TRUE,
                              prob = c(0.6, 0.4)))
)

# Ensure bounds
jjdotplotstats_qol$qol_score <- pmin(100, pmax(0, jjdotplotstats_qol$qol_score))


# ═══════════════════════════════════════════════════════════
# Dataset 8: Lab Values (jjdotplotstats_labvalues)
# ═══════════════════════════════════════════════════════════
# Clinical laboratory measurements

n_lab <- 180

jjdotplotstats_labvalues <- tibble(
  subject_id = 1:n_lab,

  # Hemoglobin (g/dL)
  # Normal: 14, Mild anemia: 11, Moderate anemia: 9
  hemoglobin = c(
    rnorm(60, mean = 14, sd = 1.5),
    rnorm(60, mean = 11, sd = 1.2),
    rnorm(60, mean = 9, sd = 1.0)
  ),

  # Anemia severity
  anemia = factor(rep(c("Normal", "Mild", "Moderate"), each = 60),
                  levels = c("Normal", "Mild", "Moderate")),

  # Gender (affects normal ranges)
  sex = factor(sample(c("Male", "Female"), n_lab, replace = TRUE))
)

# Adjust for sex (males typically higher hemoglobin)
sex_adjustment <- ifelse(jjdotplotstats_labvalues$sex == "Male", 0.5, -0.5)
jjdotplotstats_labvalues$hemoglobin <- jjdotplotstats_labvalues$hemoglobin + sex_adjustment

# Ensure realistic bounds (6-18 g/dL)
jjdotplotstats_labvalues$hemoglobin <- pmin(18, pmax(6, jjdotplotstats_labvalues$hemoglobin))


# ═══════════════════════════════════════════════════════════
# Save all datasets in multiple formats
# ═══════════════════════════════════════════════════════════

# Dataset 1: Clinical response
save(jjdotplotstats_test, file = here("data", "jjdotplotstats_test.rda"))
write.csv(jjdotplotstats_test, file = here("data", "jjdotplotstats_test.csv"), row.names = FALSE)
write_xlsx(jjdotplotstats_test, path = here("data", "jjdotplotstats_test.xlsx"))
write_omv(jjdotplotstats_test, here("data", "jjdotplotstats_test.omv"))

# Dataset 2: Two-group
save(jjdotplotstats_twogroup, file = here("data", "jjdotplotstats_twogroup.rda"))
write.csv(jjdotplotstats_twogroup, file = here("data", "jjdotplotstats_twogroup.csv"), row.names = FALSE)
write_xlsx(jjdotplotstats_twogroup, path = here("data", "jjdotplotstats_twogroup.xlsx"))
write_omv(jjdotplotstats_twogroup, here("data", "jjdotplotstats_twogroup.omv"))

# Dataset 3: Four-group
save(jjdotplotstats_fourgroup, file = here("data", "jjdotplotstats_fourgroup.rda"))
write.csv(jjdotplotstats_fourgroup, file = here("data", "jjdotplotstats_fourgroup.csv"), row.names = FALSE)
write_xlsx(jjdotplotstats_fourgroup, path = here("data", "jjdotplotstats_fourgroup.xlsx"))
write_omv(jjdotplotstats_fourgroup, here("data", "jjdotplotstats_fourgroup.omv"))

# Dataset 4: Skewed data
save(jjdotplotstats_skewed, file = here("data", "jjdotplotstats_skewed.rda"))
write.csv(jjdotplotstats_skewed, file = here("data", "jjdotplotstats_skewed.csv"), row.names = FALSE)
write_xlsx(jjdotplotstats_skewed, path = here("data", "jjdotplotstats_skewed.xlsx"))
write_omv(jjdotplotstats_skewed, here("data", "jjdotplotstats_skewed.omv"))

# Dataset 5: Outliers
save(jjdotplotstats_outliers, file = here("data", "jjdotplotstats_outliers.rda"))
write.csv(jjdotplotstats_outliers, file = here("data", "jjdotplotstats_outliers.csv"), row.names = FALSE)
write_xlsx(jjdotplotstats_outliers, path = here("data", "jjdotplotstats_outliers.xlsx"))
write_omv(jjdotplotstats_outliers, here("data", "jjdotplotstats_outliers.omv"))

# Dataset 6: Reference value
save(jjdotplotstats_reference, file = here("data", "jjdotplotstats_reference.rda"))
write.csv(jjdotplotstats_reference, file = here("data", "jjdotplotstats_reference.csv"), row.names = FALSE)
write_xlsx(jjdotplotstats_reference, path = here("data", "jjdotplotstats_reference.xlsx"))
write_omv(jjdotplotstats_reference, here("data", "jjdotplotstats_reference.omv"))

# Dataset 7: Quality of life
save(jjdotplotstats_qol, file = here("data", "jjdotplotstats_qol.rda"))
write.csv(jjdotplotstats_qol, file = here("data", "jjdotplotstats_qol.csv"), row.names = FALSE)
write_xlsx(jjdotplotstats_qol, path = here("data", "jjdotplotstats_qol.xlsx"))
write_omv(jjdotplotstats_qol, here("data", "jjdotplotstats_qol.omv"))

# Dataset 8: Lab values
save(jjdotplotstats_labvalues, file = here("data", "jjdotplotstats_labvalues.rda"))
write.csv(jjdotplotstats_labvalues, file = here("data", "jjdotplotstats_labvalues.csv"), row.names = FALSE)
write_xlsx(jjdotplotstats_labvalues, path = here("data", "jjdotplotstats_labvalues.xlsx"))
write_omv(jjdotplotstats_labvalues, here("data", "jjdotplotstats_labvalues.omv"))


# ═══════════════════════════════════════════════════════════
# Generate summary documentation
# ═══════════════════════════════════════════════════════════

summary_text <- "═══════════════════════════════════════════════════════════
JJDOTPLOTSTATS TEST DATA SUMMARY
═══════════════════════════════════════════════════════════

Dataset: jjdotplotstats (Dot Chart for Group Comparisons)
Generated: 2026-01-05
Seed: 42

DIMENSIONS
----------
Main Dataset (jjdotplotstats_test):
  Observations: 120 (40 per group)
  Groups: 3 (Control, Treatment A, Treatment B)
  Outcome: Tumor reduction percentage (0-100%)
  Grouping variables: hospital, tumor_stage
  Missing data: ~3%

Two-Group Dataset (jjdotplotstats_twogroup):
  Observations: 80 (40 per group)
  Groups: 2 (Pre-treatment, Post-treatment)
  Outcome: Pain score (0-10 VAS)
  Grouping variable: gender

Four-Group Dataset (jjdotplotstats_fourgroup):
  Observations: 160 (40 per group)
  Groups: 4 (Placebo, Low, Medium, High dose)
  Outcome: Efficacy score (0-100)
  Grouping variable: age_group

Skewed Data (jjdotplotstats_skewed):
  Observations: 90 (45 per group)
  Groups: 2 (Control, Active)
  Outcome: Biomarker (log-normal distribution)
  Grouping variable: disease

Outlier Data (jjdotplotstats_outliers):
  Observations: 100 (50 per group)
  Groups: 2 (Group A, Group B)
  Outcome: Response with 10% outliers
  Grouping variable: center

Reference Value (jjdotplotstats_reference):
  Observations: 150 (50 per group)
  Groups: 3 (Drug A, Drug B, Drug C)
  Outcome: BP reduction (mmHg)
  Reference threshold: 10 mmHg
  Grouping variable: severity

Quality of Life (jjdotplotstats_qol):
  Observations: 120 (40 per group)
  Groups: 3 (Standard, Intervention A, Intervention B)
  Outcome: QoL score (0-100)
  Grouping variable: comorbidity

Lab Values (jjdotplotstats_labvalues):
  Observations: 180 (60 per group)
  Groups: 3 (Normal, Mild anemia, Moderate anemia)
  Outcome: Hemoglobin (g/dL)
  Grouping variable: sex

VARIABLE DESCRIPTIONS
---------------------

Main Dataset (Clinical Response):
  • tumor_reduction [numeric, 0-100]: Percentage tumor reduction
  • treatment [factor]: Treatment group (Control, Treatment A, Treatment B)
  • hospital [factor]: Hospital site (Site A, B, C)
  • tumor_stage [factor]: Tumor stage (Early, Advanced)

Two-Group Dataset:
  • pain_score [numeric, 0-10]: Pain intensity (VAS scale)
  • timepoint [factor]: Assessment time (Pre-treatment, Post-treatment)
  • gender [factor]: Patient gender (Male, Female)

Four-Group Dataset:
  • efficacy_score [numeric, 0-100]: Treatment efficacy measure
  • dose [factor]: Dose level (Placebo, Low, Medium, High)
  • age_group [factor]: Age category (Young, Middle, Elderly)

Skewed Data:
  • biomarker_level [numeric, >0]: Tumor marker (log-normal)
  • treatment [factor]: Treatment (Control, Active)
  • disease [factor]: Disease type (Type 1, Type 2)

Outlier Data:
  • response [numeric]: Response variable with outliers
  • group [factor]: Group assignment (Group A, Group B)
  • center [factor]: Study center (Center 1, Center 2)

Reference Value:
  • bp_reduction [numeric]: Blood pressure reduction (mmHg)
  • drug [factor]: Drug (Drug A, B, C)
  • severity [factor]: Baseline severity (Mild, Moderate, Severe)
  • Reference: 10 mmHg (clinically significant threshold)

Quality of Life:
  • qol_score [numeric, 0-100]: Quality of life score
  • intervention [factor]: Intervention type
  • comorbidity [factor]: Comorbidity status (None, Present)

Lab Values:
  • hemoglobin [numeric, 6-18]: Hemoglobin concentration (g/dL)
  • anemia [factor]: Anemia severity (Normal, Mild, Moderate)
  • sex [factor]: Sex (Male, Female)

STATISTICAL APPROACHES COVERED
-------------------------------

Parametric (typestatistics = 'parametric'):
  • Independent samples t-test (2 groups)
  • One-way ANOVA (3+ groups)
  • Assumes normality and homogeneity of variance
  • Example: QoL scores, tumor reduction percentages

Nonparametric (typestatistics = 'nonparametric'):
  • Mann-Whitney U test (2 groups)
  • Kruskal-Wallis test (3+ groups)
  • Distribution-free, rank-based
  • Example: Biomarker data (log-normal)

Robust (typestatistics = 'robust'):
  • Yuen's trimmed means test
  • Resistant to outliers (10-20% trimming)
  • Example: Outlier-contaminated response data

Bayesian (typestatistics = 'bayes'):
  • Bayesian t-test with Bayes Factor
  • Evidence quantification (BF > 3 moderate, BF > 10 strong)
  • Example: Any continuous group comparison

EFFECT SIZES
------------

Cohen's d (effsizetype = 'biased'):
  • Standardized mean difference
  • Small: 0.2, Medium: 0.5, Large: 0.8
  • Most common in literature

Hedge's g (effsizetype = 'unbiased'):
  • Bias-corrected for small samples
  • Recommended for n < 50 per group

Eta-squared (effsizetype = 'eta'):
  • Proportion of variance explained
  • Small: 0.01, Medium: 0.06, Large: 0.14

Omega-squared (effsizetype = 'omega'):
  • Adjusted eta-squared
  • Less biased estimate

SPECIAL FEATURES
----------------

Test Value Testing (testvalue, testvalueline):
  • Test against clinically meaningful threshold
  • Example: BP reduction ≥ 10 mmHg (reference dataset)
  • Null hypothesis: mean = test value

Central Tendency Lines (centralityplotting):
  • Mean (centralitytype = 'parametric')
  • Median (centralitytype = 'nonparametric')
  • Trimmed mean (centralitytype = 'robust')
  • Bayesian MAP (centralitytype = 'bayes')

Grouped Analysis (grvar):
  • Separate plots for each subgroup
  • Examples: By hospital, gender, age group, severity
  • Tests interaction/heterogeneity

Central Tendency Display (centralityparameter):
  • mean: Vertical line at group mean
  • median: Vertical line at group median
  • none: No central tendency line

DATA CHARACTERISTICS
--------------------

Main Dataset (Tumor Reduction):
  • Control: 20% ± 15% (poor response)
  • Treatment A: 45% ± 18% (moderate response)
  • Treatment B: 65% ± 20% (good response)
  • Clear dose-response relationship

Two-Group (Pain):
  • Pre: 7.5 ± 2.0 (high pain)
  • Post: 3.5 ± 1.8 (low pain)
  • Large effect size (d ≈ 2.2)

Four-Group (Efficacy):
  • Placebo: 35 ± 12
  • Low dose: 50 ± 14
  • Medium dose: 65 ± 13
  • High dose: 75 ± 15
  • Dose-response gradient

Skewed (Biomarker):
  • Control: median ~8 (right-skewed)
  • Active: median ~4 (treatment effect)
  • Log-normal distribution

Outliers (Response):
  • 10% extreme values per group
  • Tests robust methods

Reference (BP Reduction):
  • Drug A: 8 mmHg (below threshold)
  • Drug B: 15 mmHg (above threshold)
  • Drug C: 12 mmHg (above threshold)
  • Target: ≥10 mmHg reduction

EXAMPLE R CODE
--------------

# Load test data
data(jjdotplotstats_test, package = 'ClinicoPath')

# 1. Basic three-group comparison
jjdotplotstats(
  data = jjdotplotstats_test,
  dep = 'tumor_reduction',
  group = 'treatment',
  typestatistics = 'parametric'
)

# 2. With centrality lines and statistical subtitle
jjdotplotstats(
  data = jjdotplotstats_test,
  dep = 'tumor_reduction',
  group = 'treatment',
  centralityplotting = TRUE,
  centralitytype = 'parametric',
  resultssubtitle = TRUE,
  mytitle = 'Tumor Response by Treatment'
)

# 3. Two-group pre-post comparison
data(jjdotplotstats_twogroup)
jjdotplotstats(
  data = jjdotplotstats_twogroup,
  dep = 'pain_score',
  group = 'timepoint',
  typestatistics = 'parametric',
  centralityplotting = TRUE,
  xtitle = 'Pain Score (0-10 VAS)',
  ytitle = 'Assessment Time'
)

# 4. Four-group dose-response
data(jjdotplotstats_fourgroup)
jjdotplotstats(
  data = jjdotplotstats_fourgroup,
  dep = 'efficacy_score',
  group = 'dose',
  typestatistics = 'parametric',
  effsizetype = 'unbiased',
  resultssubtitle = TRUE
)

# 5. Nonparametric for skewed data
data(jjdotplotstats_skewed)
jjdotplotstats(
  data = jjdotplotstats_skewed,
  dep = 'biomarker_level',
  group = 'treatment',
  typestatistics = 'nonparametric',
  centralityplotting = TRUE,
  centralitytype = 'nonparametric'
)

# 6. Robust statistics with outliers
data(jjdotplotstats_outliers)
jjdotplotstats(
  data = jjdotplotstats_outliers,
  dep = 'response',
  group = 'group',
  typestatistics = 'robust',
  centralityplotting = TRUE,
  centralitytype = 'robust'
)

# 7. Testing against reference value
data(jjdotplotstats_reference)
jjdotplotstats(
  data = jjdotplotstats_reference,
  dep = 'bp_reduction',
  group = 'drug',
  testvalue = 10,  # Clinical threshold
  testvalueline = TRUE,
  mytitle = 'BP Reduction vs Target (10 mmHg)'
)

# 8. Grouped analysis by hospital
jjdotplotstats(
  data = jjdotplotstats_test,
  dep = 'tumor_reduction',
  group = 'treatment',
  grvar = 'hospital',
  centralityplotting = TRUE
)

# 9. Bayesian analysis
jjdotplotstats(
  data = jjdotplotstats_test,
  dep = 'tumor_reduction',
  group = 'treatment',
  typestatistics = 'bayes',
  bfmessage = TRUE
)

# 10. Publication-ready figure
jjdotplotstats(
  data = jjdotplotstats_test,
  dep = 'tumor_reduction',
  group = 'treatment',
  typestatistics = 'parametric',
  centralityplotting = TRUE,
  centralitytype = 'parametric',
  resultssubtitle = TRUE,
  mytitle = 'Tumor Response Across Treatment Arms',
  xtitle = 'Tumor Reduction (%)',
  ytitle = 'Treatment Group',
  conflevel = 0.95,
  k = 2,
  plotwidth = 800,
  plotheight = 600
)

FILES GENERATED
---------------
  ✓ data/jjdotplotstats_test.rda           (Main 3-group comparison)
  ✓ data/jjdotplotstats_twogroup.rda       (Pre-post/two-group)
  ✓ data/jjdotplotstats_fourgroup.rda      (Four dose levels)
  ✓ data/jjdotplotstats_skewed.rda         (Log-normal biomarker)
  ✓ data/jjdotplotstats_outliers.rda       (Outlier-contaminated)
  ✓ data/jjdotplotstats_reference.rda      (Reference value testing)
  ✓ data/jjdotplotstats_qol.rda            (Quality of life)
  ✓ data/jjdotplotstats_labvalues.rda      (Hemoglobin levels)
  ✓ CSV, XLSX, OMV formats for all datasets

═══════════════════════════════════════════════════════════

✓ All test data files generated successfully!
✓ Summary saved to: JJDOTPLOTSTATS_TEST_DATA_SUMMARY.md

✓ jjdotplotstats test data generation complete!
"

cat(summary_text)
writeLines(summary_text, here("JJDOTPLOTSTATS_TEST_DATA_SUMMARY.md"))

cat("\n✓ jjdotplotstats test data generation complete!\n")
