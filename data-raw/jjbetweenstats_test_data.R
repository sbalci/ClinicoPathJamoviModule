# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjbetweenstats
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the jjbetweenstats jamovi function
# (box-violin plots for continuous vs categorical comparisons)
#
# Generated: 2026-01-05
# Seed: 42
# Observations: 300

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══════════════════════════════════════════════════════════
# 1. Comprehensive Clinical Trial Dataset
# ═══════════════════════════════════════════════════════════

n <- 300

# Generate base clinical trial data
jjbetweenstats_test <- tibble(
  # Patient ID
  patient_id = 1:n,

  # Treatment group (3 levels)
  treatment = sample(
    c("Placebo", "Low Dose", "High Dose"),
    n,
    replace = TRUE,
    prob = c(0.35, 0.35, 0.30)
  ),

  # Tumor stage (4 levels)
  tumor_stage = sample(
    c("Stage I", "Stage II", "Stage III", "Stage IV"),
    n,
    replace = TRUE,
    prob = c(0.20, 0.30, 0.30, 0.20)
  ),

  # Sex (2 levels)
  sex = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)),

  # Age group (3 levels)
  age_group = sample(
    c("<50", "50-65", ">65"),
    n,
    replace = TRUE,
    prob = c(0.25, 0.45, 0.30)
  )
)

# Generate continuous outcomes with realistic treatment effects
# Initialize columns
jjbetweenstats_test$tumor_reduction <- NA_real_
jjbetweenstats_test$pain_score <- NA_real_
jjbetweenstats_test$qol_score <- NA_real_
jjbetweenstats_test$biomarker_level <- NA_real_
jjbetweenstats_test$crp_level <- NA_real_
jjbetweenstats_test$ldh_level <- NA_real_

# Generate values based on treatment group
for (i in 1:n) {
  trt <- jjbetweenstats_test$treatment[i]

  # Tumor reduction (mm, higher is better)
  if (trt == "Placebo") {
    jjbetweenstats_test$tumor_reduction[i] <- rnorm(1, mean = 10, sd = 8)
  } else if (trt == "Low Dose") {
    jjbetweenstats_test$tumor_reduction[i] <- rnorm(1, mean = 25, sd = 10)
  } else {  # High Dose
    jjbetweenstats_test$tumor_reduction[i] <- rnorm(1, mean = 40, sd = 12)
  }

  # Pain score (0-100, lower is better - VAS)
  if (trt == "Placebo") {
    jjbetweenstats_test$pain_score[i] <- rbeta(1, 5, 3) * 100
  } else if (trt == "Low Dose") {
    jjbetweenstats_test$pain_score[i] <- rbeta(1, 4, 4) * 100
  } else {
    jjbetweenstats_test$pain_score[i] <- rbeta(1, 3, 5) * 100
  }

  # Quality of life score (0-100, higher is better)
  if (trt == "Placebo") {
    jjbetweenstats_test$qol_score[i] <- rnorm(1, mean = 50, sd = 15)
  } else if (trt == "Low Dose") {
    jjbetweenstats_test$qol_score[i] <- rnorm(1, mean = 60, sd = 14)
  } else {
    jjbetweenstats_test$qol_score[i] <- rnorm(1, mean = 70, sd = 13)
  }

  # Biomarker level (higher with better response)
  if (trt == "Placebo") {
    jjbetweenstats_test$biomarker_level[i] <- rnorm(1, mean = 50, sd = 15)
  } else if (trt == "Low Dose") {
    jjbetweenstats_test$biomarker_level[i] <- rnorm(1, mean = 65, sd = 16)
  } else {
    jjbetweenstats_test$biomarker_level[i] <- rnorm(1, mean = 80, sd = 18)
  }

  # C-reactive protein (log-normal, lower is better)
  if (trt == "Placebo") {
    jjbetweenstats_test$crp_level[i] <- rlnorm(1, meanlog = 2.5, sdlog = 0.8)
  } else if (trt == "Low Dose") {
    jjbetweenstats_test$crp_level[i] <- rlnorm(1, meanlog = 2.0, sdlog = 0.7)
  } else {
    jjbetweenstats_test$crp_level[i] <- rlnorm(1, meanlog = 1.5, sdlog = 0.6)
  }

  # LDH level (exponential, lower is better)
  if (trt == "Placebo") {
    jjbetweenstats_test$ldh_level[i] <- rexp(1, rate = 1/300) + 200
  } else if (trt == "Low Dose") {
    jjbetweenstats_test$ldh_level[i] <- rexp(1, rate = 1/250) + 180
  } else {
    jjbetweenstats_test$ldh_level[i] <- rexp(1, rate = 1/200) + 160
  }
}

# Add age as continuous variable
jjbetweenstats_test$age <- round(rnorm(n, mean = 62, sd = 12))

# Add BMI as continuous variable
jjbetweenstats_test$bmi <- rnorm(n, mean = 26, sd = 4.5)

# Convert to factors with proper ordering
jjbetweenstats_test <- jjbetweenstats_test %>%
  mutate(
    treatment = factor(treatment, levels = c("Placebo", "Low Dose", "High Dose")),
    tumor_stage = factor(tumor_stage, levels = c("Stage I", "Stage II", "Stage III", "Stage IV")),
    sex = factor(sex, levels = c("Male", "Female")),
    age_group = factor(age_group, levels = c("<50", "50-65", ">65"))
  )

# Add some missing data (~3%)
n_missing <- round(n * 0.03)
jjbetweenstats_test$biomarker_level[sample(n, n_missing)] <- NA
jjbetweenstats_test$qol_score[sample(n, n_missing)] <- NA


# ═══════════════════════════════════════════════════════════
# 2. Two-Group Comparison Dataset
# ═══════════════════════════════════════════════════════════

n_two <- 150

jjbetweenstats_twogroup <- tibble(
  patient_id = 1:n_two,

  # Binary grouping
  group = sample(c("Control", "Treatment"), n_two, replace = TRUE, prob = c(0.5, 0.5)),

  # Continuous outcome
  outcome = NA_real_,

  # Secondary outcome
  secondary_outcome = NA_real_,

  # Sex for stratification
  sex = sample(c("Male", "Female"), n_two, replace = TRUE)
)

# Generate outcomes based on group
for (i in 1:n_two) {
  if (jjbetweenstats_twogroup$group[i] == "Control") {
    jjbetweenstats_twogroup$outcome[i] <- rnorm(1, mean = 50, sd = 15)
    jjbetweenstats_twogroup$secondary_outcome[i] <- rnorm(1, mean = 100, sd = 20)
  } else {
    jjbetweenstats_twogroup$outcome[i] <- rnorm(1, mean = 65, sd = 15)
    jjbetweenstats_twogroup$secondary_outcome[i] <- rnorm(1, mean = 120, sd = 20)
  }
}

jjbetweenstats_twogroup <- jjbetweenstats_twogroup %>%
  mutate(
    group = factor(group, levels = c("Control", "Treatment")),
    sex = factor(sex, levels = c("Male", "Female"))
  )


# ═══════════════════════════════════════════════════════════
# 3. Dataset with Outliers (for robust statistics)
# ═══════════════════════════════════════════════════════════

jjbetweenstats_outliers <- jjbetweenstats_test

# Add outliers to tumor_reduction (~10% outliers)
n_outliers <- round(n * 0.10)
outlier_indices <- sample(n, n_outliers)
jjbetweenstats_outliers$tumor_reduction[outlier_indices] <-
  jjbetweenstats_outliers$tumor_reduction[outlier_indices] + rnorm(n_outliers, mean = 50, sd = 15)

# Add outliers to pain_score
jjbetweenstats_outliers$pain_score[sample(n, n_outliers)] <-
  sample(c(0, 100), n_outliers, replace = TRUE)


# ═══════════════════════════════════════════════════════════
# 4. Skewed Data (for nonparametric tests)
# ═══════════════════════════════════════════════════════════

n_skewed <- 200

jjbetweenstats_skewed <- tibble(
  patient_id = 1:n_skewed,

  treatment = sample(
    c("Placebo", "Low Dose", "High Dose"),
    n_skewed,
    replace = TRUE
  ),

  # Log-normal outcome (right-skewed)
  tumor_marker = NA_real_,

  # Exponential outcome (right-skewed)
  time_to_response = NA_real_
)

# Generate skewed outcomes
for (i in 1:n_skewed) {
  trt <- jjbetweenstats_skewed$treatment[i]

  if (trt == "Placebo") {
    jjbetweenstats_skewed$tumor_marker[i] <- rlnorm(1, meanlog = 4, sdlog = 1)
    jjbetweenstats_skewed$time_to_response[i] <- rexp(1, rate = 1/30)
  } else if (trt == "Low Dose") {
    jjbetweenstats_skewed$tumor_marker[i] <- rlnorm(1, meanlog = 3.5, sdlog = 0.9)
    jjbetweenstats_skewed$time_to_response[i] <- rexp(1, rate = 1/20)
  } else {
    jjbetweenstats_skewed$tumor_marker[i] <- rlnorm(1, meanlog = 3, sdlog = 0.8)
    jjbetweenstats_skewed$time_to_response[i] <- rexp(1, rate = 1/15)
  }
}

jjbetweenstats_skewed <- jjbetweenstats_skewed %>%
  mutate(
    treatment = factor(treatment, levels = c("Placebo", "Low Dose", "High Dose"))
  )


# ═══════════════════════════════════════════════════════════
# 5. Four-Group Dataset (for multi-level comparisons)
# ═══════════════════════════════════════════════════════════

n_four <- 240

jjbetweenstats_fourgroup <- tibble(
  patient_id = 1:n_four,

  # Four treatment groups
  treatment = sample(
    c("Placebo", "Drug A", "Drug B", "Combination"),
    n_four,
    replace = TRUE
  ),

  # Continuous outcome
  efficacy_score = NA_real_,

  # Age group for stratification
  age_group = sample(c("Young", "Middle", "Elderly"), n_four, replace = TRUE)
)

# Generate outcomes with realistic differences
for (i in 1:n_four) {
  trt <- jjbetweenstats_fourgroup$treatment[i]

  if (trt == "Placebo") {
    jjbetweenstats_fourgroup$efficacy_score[i] <- rnorm(1, mean = 40, sd = 12)
  } else if (trt == "Drug A") {
    jjbetweenstats_fourgroup$efficacy_score[i] <- rnorm(1, mean = 55, sd = 13)
  } else if (trt == "Drug B") {
    jjbetweenstats_fourgroup$efficacy_score[i] <- rnorm(1, mean = 60, sd = 14)
  } else {  # Combination
    jjbetweenstats_fourgroup$efficacy_score[i] <- rnorm(1, mean = 75, sd = 12)
  }
}

jjbetweenstats_fourgroup <- jjbetweenstats_fourgroup %>%
  mutate(
    treatment = factor(treatment, levels = c("Placebo", "Drug A", "Drug B", "Combination")),
    age_group = factor(age_group, levels = c("Young", "Middle", "Elderly"))
  )


# ═══════════════════════════════════════════════════════════
# Save All Datasets
# ═══════════════════════════════════════════════════════════

# 1. Main comprehensive dataset
save(jjbetweenstats_test, file = here("data", "jjbetweenstats_test.rda"))
write.csv(jjbetweenstats_test, file = here("data", "jjbetweenstats_test.csv"), row.names = FALSE)
writexl::write_xlsx(
  list(
    comprehensive = jjbetweenstats_test,
    twogroup = jjbetweenstats_twogroup,
    outliers = jjbetweenstats_outliers,
    skewed = jjbetweenstats_skewed,
    fourgroup = jjbetweenstats_fourgroup
  ),
  path = here("data", "jjbetweenstats_test.xlsx")
)
jmvReadWrite::write_omv(jjbetweenstats_test, here("data", "jjbetweenstats_test.omv"))

# 2. Individual datasets
save(jjbetweenstats_twogroup, file = here("data", "jjbetweenstats_twogroup.rda"))
save(jjbetweenstats_outliers, file = here("data", "jjbetweenstats_outliers.rda"))
save(jjbetweenstats_skewed, file = here("data", "jjbetweenstats_skewed.rda"))
save(jjbetweenstats_fourgroup, file = here("data", "jjbetweenstats_fourgroup.rda"))


# ═══════════════════════════════════════════════════════════
# Generate Summary Documentation
# ═══════════════════════════════════════════════════════════

summary_text <- paste0("
═══════════════════════════════════════════════════════════
JJBETWEENSTATS TEST DATA SUMMARY
═══════════════════════════════════════════════════════════

Dataset: jjbetweenstats_test (Box-Violin Plots for Between-Groups)
Generated: ", Sys.Date(), "
Seed: 42

DIMENSIONS
----------
Main Dataset (jjbetweenstats_test):
  Observations: ", n, "
  Variables: ", ncol(jjbetweenstats_test), "
  Treatment groups: 3 (Placebo, Low/High Dose)
  Missing data: ~3% in biomarker_level and qol_score

Two-Group Dataset (jjbetweenstats_twogroup):
  Observations: ", n_two, "
  Groups: 2 (Control, Treatment)
  Variables: 5

Outliers Dataset (jjbetweenstats_outliers):
  Observations: ", n, "
  Outliers: ~10% in tumor_reduction and pain_score
  For robust statistics testing

Skewed Data (jjbetweenstats_skewed):
  Observations: ", n_skewed, "
  Distributions: Log-normal and exponential
  For nonparametric testing

Four-Group Dataset (jjbetweenstats_fourgroup):
  Observations: ", n_four, "
  Groups: 4 (Placebo, Drug A, Drug B, Combination)
  For multi-level comparisons

VARIABLE DESCRIPTIONS
---------------------

Continuous Outcomes (for dep parameter):
  • tumor_reduction [numeric, ≥0]: Tumor size reduction in mm
  • pain_score [numeric, 0-100]: Visual analog pain scale (lower is better)
  • qol_score [numeric, 0-100]: Quality of life score (higher is better)
  • biomarker_level [numeric]: Biomarker concentration
  • crp_level [numeric, ≥0]: C-reactive protein (log-normal, lower is better)
  • ldh_level [numeric, ≥0]: Lactate dehydrogenase (exponential distribution)
  • age [integer]: Patient age in years
  • bmi [numeric]: Body mass index

Grouping Variables (for group parameter):
  • treatment [factor, 3 levels]: Placebo, Low Dose, High Dose
  • tumor_stage [factor, 4 levels]: Stage I, Stage II, Stage III, Stage IV (ordinal)
  • group [factor, 2 levels]: Control, Treatment (two-group dataset)
  • sex [factor, 2 levels]: Male, Female

Split-By Variables (for grvar parameter):
  • sex [factor, 2 levels]: Male, Female
  • age_group [factor, 3 levels]: <50, 50-65, >65
  • tumor_stage [factor, 4 levels]: Stage I-IV

STATISTICAL APPROACHES COVERED
-------------------------------

Parametric (typestatistics = 'parametric'):
  • Independent t-test (2 groups)
  • One-way ANOVA (3+ groups)
  • Welch's t-test/ANOVA (unequal variances)
  • Example: tumor_reduction, qol_score (approximately normal)

Nonparametric (typestatistics = 'nonparametric'):
  • Mann-Whitney U test (2 groups)
  • Kruskal-Wallis test (3+ groups)
  • Example: pain_score (beta), crp_level (log-normal), skewed dataset

Robust (typestatistics = 'robust'):
  • Trimmed means (20% trim by default)
  • Resistant to outliers
  • Example: outliers dataset

Bayesian (typestatistics = 'bayes'):
  • Bayesian t-test (2 groups)
  • Bayesian ANOVA (3+ groups)
  • Bayes Factor for evidence assessment
  • Example: Any continuous outcome

DATA CHARACTERISTICS
--------------------

Treatment Effects (Dose-Response):
  • Tumor reduction: Placebo (10mm) < Low (25mm) < High (40mm)
  • Pain scores: Decreasing trend with dose (beta distributed)
  • QoL scores: Placebo (50) < Low (60) < High (70)
  • Biomarker levels: Increasing with treatment response
  • CRP levels: Decreasing with treatment (log-normal)
  • LDH levels: Decreasing with treatment (exponential)

Two-Group Comparison:
  • Clear separation between Control and Treatment
  • Effect size: Cohen's d ≈ 1.0 (large effect)
  • Normal distributions suitable for t-test

Outliers Dataset:
  • 10% contamination with extreme values
  • Tests robust statistics resistance to outliers
  • Demonstrates advantage of trimmed means

Skewed Data:
  • Right-skewed tumor markers (log-normal)
  • Exponential time-to-response
  • Demonstrates need for nonparametric tests
  • Violates normality assumption

Four-Group Dataset:
  • Progressive efficacy: Placebo < Drug A < Drug B < Combination
  • Synergistic effect in combination group
  • Suitable for ANOVA with post-hoc tests

EXAMPLE R CODE
--------------

# Load test data
data(jjbetweenstats_test, package = 'ClinicoPath')

# 1. Basic parametric comparison (3 groups)
jjbetweenstats(
  data = jjbetweenstats_test,
  dep = 'tumor_reduction',
  group = 'treatment',
  typestatistics = 'parametric'
)

# 2. With pairwise comparisons
jjbetweenstats(
  data = jjbetweenstats_test,
  dep = 'tumor_reduction',
  group = 'treatment',
  pairwisecomparisons = TRUE,
  padjustmethod = 'bonferroni'
)

# 3. Split by sex (grouped analysis)
jjbetweenstats(
  data = jjbetweenstats_test,
  dep = 'qol_score',
  group = 'treatment',
  grvar = 'sex',
  typestatistics = 'parametric'
)

# 4. Multiple dependent variables
jjbetweenstats(
  data = jjbetweenstats_test,
  dep = c('tumor_reduction', 'pain_score', 'qol_score'),
  group = 'treatment',
  pairwisecomparisons = TRUE
)

# 5. Two-group comparison (t-test)
data(jjbetweenstats_twogroup)
jjbetweenstats(
  data = jjbetweenstats_twogroup,
  dep = 'outcome',
  group = 'group',
  typestatistics = 'parametric'
)

# 6. Nonparametric test for skewed data
data(jjbetweenstats_skewed)
jjbetweenstats(
  data = jjbetweenstats_skewed,
  dep = 'tumor_marker',
  group = 'treatment',
  typestatistics = 'nonparametric'
)

# 7. Robust statistics with outliers
data(jjbetweenstats_outliers)
jjbetweenstats(
  data = jjbetweenstats_outliers,
  dep = 'tumor_reduction',
  group = 'treatment',
  typestatistics = 'robust'
)

# 8. Bayesian analysis
jjbetweenstats(
  data = jjbetweenstats_test,
  dep = 'biomarker_level',
  group = 'treatment',
  typestatistics = 'bayes',
  bfmessage = TRUE
)

# 9. Four-group comparison with ANOVA
data(jjbetweenstats_fourgroup)
jjbetweenstats(
  data = jjbetweenstats_fourgroup,
  dep = 'efficacy_score',
  group = 'treatment',
  pairwisecomparisons = TRUE,
  padjustmethod = 'holm'
)

# 10. With centrality measures
jjbetweenstats(
  data = jjbetweenstats_test,
  dep = 'qol_score',
  group = 'treatment',
  centralityplotting = TRUE,
  centralitytype = 'parametric'  # Show means
)

# 11. Publication-ready with ggpubr
jjbetweenstats(
  data = jjbetweenstats_test,
  dep = 'tumor_reduction',
  group = 'treatment',
  addGGPubrPlot = TRUE,
  ggpubrPlotType = 'boxviolin',
  ggpubrPalette = 'jco'
)

# 12. Multiple endpoint correction guidance
jjbetweenstats(
  data = jjbetweenstats_test,
  dep = c('tumor_reduction', 'pain_score', 'qol_score'),
  group = 'treatment',
  multiEndpointCorrection = 'bonferroni'
)

FILES GENERATED
---------------
  ✓ data/jjbetweenstats_test.rda          (Comprehensive dataset)
  ✓ data/jjbetweenstats_twogroup.rda      (Two-group comparison)
  ✓ data/jjbetweenstats_outliers.rda      (With outliers)
  ✓ data/jjbetweenstats_skewed.rda        (Skewed distributions)
  ✓ data/jjbetweenstats_fourgroup.rda     (Four treatment groups)
  ✓ data/jjbetweenstats_test.csv          (CSV format)
  ✓ data/jjbetweenstats_test.xlsx         (Excel with multiple sheets)
  ✓ data/jjbetweenstats_test.omv          (Jamovi format)

═══════════════════════════════════════════════════════════

✓ All test data files generated successfully!
✓ Summary saved to: JJBETWEENSTATS_TEST_DATA_SUMMARY.md
")

cat(summary_text)
writeLines(summary_text, here("JJBETWEENSTATS_TEST_DATA_SUMMARY.md"))

cat("\n✓ jjbetweenstats test data generation complete!\n")
