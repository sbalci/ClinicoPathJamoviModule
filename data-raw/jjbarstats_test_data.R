# ═══════════════════════════════════════════════════════════
# Test Data Generation: jjbarstats
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the jjbarstats jamovi function
# (categorical vs categorical bar charts with chi-square tests)
#
# Generated: 2026-01-05
# Seed: 42
# Observations: 300

library(tibble)
library(dplyr)
library(here)
set.seed(42)

# ═══════════════════════════════════════════════════════════
# 1. Comprehensive Test Dataset (Multiple Scenarios)
# ═══════════════════════════════════════════════════════════

n <- 300

# Generate comprehensive clinical trial data
jjbarstats_test <- tibble(
  # Patient ID
  patient_id = 1:n,

  # Treatment group (3 levels)
  treatment = sample(
    c("Placebo", "Low Dose", "High Dose"),
    n,
    replace = TRUE,
    prob = c(0.35, 0.35, 0.30)
  ),

  # Response status (3 levels - ordinal)
  response = NA_character_,

  # Disease status (2 levels - binary outcome)
  disease_status = NA_character_,

  # Tumor stage (4 levels - ordinal)
  tumor_stage = sample(
    c("Stage I", "Stage II", "Stage III", "Stage IV"),
    n,
    replace = TRUE,
    prob = c(0.20, 0.30, 0.30, 0.20)
  ),

  # Biomarker expression (3 levels - ordinal)
  biomarker_expression = sample(
    c("Negative", "Low", "High"),
    n,
    replace = TRUE,
    prob = c(0.40, 0.35, 0.25)
  ),

  # Sex (2 levels)
  sex = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)),

  # Age group (3 levels)
  age_group = sample(
    c("<50", "50-65", ">65"),
    n,
    replace = TRUE,
    prob = c(0.25, 0.45, 0.30)
  ),

  # Risk factor exposure (2 levels)
  smoking_status = sample(
    c("Non-smoker", "Smoker"),
    n,
    replace = TRUE,
    prob = c(0.65, 0.35)
  ),

  # Comorbidity (2 levels)
  comorbidity = sample(
    c("Absent", "Present"),
    n,
    replace = TRUE,
    prob = c(0.70, 0.30)
  )
)

# Generate response based on treatment (realistic effect)
for (i in 1:n) {
  if (jjbarstats_test$treatment[i] == "Placebo") {
    jjbarstats_test$response[i] <- sample(
      c("No Response", "Partial Response", "Complete Response"),
      1,
      prob = c(0.60, 0.30, 0.10)
    )
  } else if (jjbarstats_test$treatment[i] == "Low Dose") {
    jjbarstats_test$response[i] <- sample(
      c("No Response", "Partial Response", "Complete Response"),
      1,
      prob = c(0.40, 0.40, 0.20)
    )
  } else {  # High Dose
    jjbarstats_test$response[i] <- sample(
      c("No Response", "Partial Response", "Complete Response"),
      1,
      prob = c(0.20, 0.40, 0.40)
    )
  }
}

# Generate disease status based on risk factors
for (i in 1:n) {
  base_prob <- 0.30
  if (jjbarstats_test$smoking_status[i] == "Smoker") base_prob <- base_prob + 0.20
  if (jjbarstats_test$comorbidity[i] == "Present") base_prob <- base_prob + 0.15

  jjbarstats_test$disease_status[i] <- sample(
    c("Disease-Free", "Recurrence"),
    1,
    prob = c(1 - base_prob, base_prob)
  )
}

# Convert to factors with proper ordering
jjbarstats_test <- jjbarstats_test %>%
  mutate(
    treatment = factor(treatment, levels = c("Placebo", "Low Dose", "High Dose")),
    response = factor(response, levels = c("No Response", "Partial Response", "Complete Response")),
    disease_status = factor(disease_status, levels = c("Disease-Free", "Recurrence")),
    tumor_stage = factor(tumor_stage, levels = c("Stage I", "Stage II", "Stage III", "Stage IV")),
    biomarker_expression = factor(biomarker_expression, levels = c("Negative", "Low", "High")),
    sex = factor(sex, levels = c("Male", "Female")),
    age_group = factor(age_group, levels = c("<50", "50-65", ">65")),
    smoking_status = factor(smoking_status, levels = c("Non-smoker", "Smoker")),
    comorbidity = factor(comorbidity, levels = c("Absent", "Present"))
  )

# Add some missing data (~3%)
n_missing <- round(n * 0.03)
jjbarstats_test$biomarker_expression[sample(n, n_missing)] <- NA


# ═══════════════════════════════════════════════════════════
# 2. Diagnostic Test Dataset (2×2 Table)
# ═══════════════════════════════════════════════════════════

n_diagnostic <- 200

jjbarstats_diagnostic <- tibble(
  patient_id = 1:n_diagnostic,

  # Gold standard diagnosis
  diagnosis = sample(
    c("Negative", "Positive"),
    n_diagnostic,
    replace = TRUE,
    prob = c(0.70, 0.30)
  ),

  # Test result (with realistic sensitivity/specificity)
  test_result = NA_character_
)

# Generate test results based on diagnosis
# Sensitivity = 0.85, Specificity = 0.90
for (i in 1:n_diagnostic) {
  if (jjbarstats_diagnostic$diagnosis[i] == "Positive") {
    # True positive or false negative
    jjbarstats_diagnostic$test_result[i] <- sample(
      c("Positive", "Negative"),
      1,
      prob = c(0.85, 0.15)
    )
  } else {
    # True negative or false positive
    jjbarstats_diagnostic$test_result[i] <- sample(
      c("Negative", "Positive"),
      1,
      prob = c(0.90, 0.10)
    )
  }
}

jjbarstats_diagnostic <- jjbarstats_diagnostic %>%
  mutate(
    diagnosis = factor(diagnosis, levels = c("Negative", "Positive")),
    test_result = factor(test_result, levels = c("Negative", "Positive"))
  )


# ═══════════════════════════════════════════════════════════
# 3. Paired/Repeated Measures Dataset (McNemar's test)
# ═══════════════════════════════════════════════════════════

n_paired <- 150

jjbarstats_paired <- tibble(
  patient_id = 1:n_paired,

  # Baseline status
  baseline_status = sample(
    c("Negative", "Positive"),
    n_paired,
    replace = TRUE,
    prob = c(0.40, 0.60)
  ),

  # Follow-up status (after treatment)
  followup_status = NA_character_,

  # Treatment arm
  treatment_arm = sample(
    c("Treatment A", "Treatment B"),
    n_paired,
    replace = TRUE
  )
)

# Generate follow-up based on baseline and treatment
for (i in 1:n_paired) {
  if (jjbarstats_paired$baseline_status[i] == "Negative") {
    # If negative at baseline, mostly stay negative
    jjbarstats_paired$followup_status[i] <- sample(
      c("Negative", "Positive"),
      1,
      prob = c(0.85, 0.15)
    )
  } else {
    # If positive at baseline, treatment effect
    if (jjbarstats_paired$treatment_arm[i] == "Treatment A") {
      jjbarstats_paired$followup_status[i] <- sample(
        c("Negative", "Positive"),
        1,
        prob = c(0.60, 0.40)  # 60% conversion to negative
      )
    } else {
      jjbarstats_paired$followup_status[i] <- sample(
        c("Negative", "Positive"),
        1,
        prob = c(0.75, 0.25)  # 75% conversion (Treatment B better)
      )
    }
  }
}

jjbarstats_paired <- jjbarstats_paired %>%
  mutate(
    baseline_status = factor(baseline_status, levels = c("Negative", "Positive")),
    followup_status = factor(followup_status, levels = c("Negative", "Positive")),
    treatment_arm = factor(treatment_arm, levels = c("Treatment A", "Treatment B"))
  )


# ═══════════════════════════════════════════════════════════
# 4. Aggregated Data with Counts
# ═══════════════════════════════════════════════════════════

# Create contingency table as aggregated data
jjbarstats_aggregated <- tibble(
  response_category = rep(c("No Response", "Partial Response", "Complete Response"), each = 3),
  treatment_group = rep(c("Placebo", "Low Dose", "High Dose"), times = 3),
  count = c(
    # No Response: Placebo, Low, High
    45, 30, 15,
    # Partial Response: Placebo, Low, High
    20, 35, 30,
    # Complete Response: Placebo, Low, High
    5, 15, 30
  )
) %>%
  mutate(
    response_category = factor(response_category,
                               levels = c("No Response", "Partial Response", "Complete Response")),
    treatment_group = factor(treatment_group,
                            levels = c("Placebo", "Low Dose", "High Dose"))
  )


# ═══════════════════════════════════════════════════════════
# 5. Biomarker Expression Dataset
# ═══════════════════════════════════════════════════════════

n_biomarker <- 250

jjbarstats_biomarker <- tibble(
  patient_id = 1:n_biomarker,

  # Patient subtype
  subtype = sample(
    c("Subtype A", "Subtype B", "Subtype C"),
    n_biomarker,
    replace = TRUE,
    prob = c(0.40, 0.35, 0.25)
  ),

  # Biomarker expression (ordinal, correlated with subtype)
  her2_status = NA_character_,
  er_status = NA_character_,
  pr_status = NA_character_
)

# Generate HER2 based on subtype
for (i in 1:n_biomarker) {
  if (jjbarstats_biomarker$subtype[i] == "Subtype A") {
    jjbarstats_biomarker$her2_status[i] <- sample(
      c("Negative", "Low", "High"),
      1,
      prob = c(0.70, 0.20, 0.10)
    )
  } else if (jjbarstats_biomarker$subtype[i] == "Subtype B") {
    jjbarstats_biomarker$her2_status[i] <- sample(
      c("Negative", "Low", "High"),
      1,
      prob = c(0.40, 0.35, 0.25)
    )
  } else {  # Subtype C
    jjbarstats_biomarker$her2_status[i] <- sample(
      c("Negative", "Low", "High"),
      1,
      prob = c(0.20, 0.30, 0.50)
    )
  }

  # ER status
  jjbarstats_biomarker$er_status[i] <- sample(
    c("Negative", "Positive"),
    1,
    prob = c(0.35, 0.65)
  )

  # PR status
  jjbarstats_biomarker$pr_status[i] <- sample(
    c("Negative", "Positive"),
    1,
    prob = c(0.40, 0.60)
  )
}

jjbarstats_biomarker <- jjbarstats_biomarker %>%
  mutate(
    subtype = factor(subtype, levels = c("Subtype A", "Subtype B", "Subtype C")),
    her2_status = factor(her2_status, levels = c("Negative", "Low", "High")),
    er_status = factor(er_status, levels = c("Negative", "Positive")),
    pr_status = factor(pr_status, levels = c("Negative", "Positive"))
  )


# ═══════════════════════════════════════════════════════════
# Save All Datasets
# ═══════════════════════════════════════════════════════════

# 1. Main comprehensive dataset
save(jjbarstats_test, file = here("data", "jjbarstats_test.rda"))
write.csv(jjbarstats_test, file = here("data", "jjbarstats_test.csv"), row.names = FALSE)
writexl::write_xlsx(
  list(
    comprehensive = jjbarstats_test,
    diagnostic = jjbarstats_diagnostic,
    paired = jjbarstats_paired,
    aggregated = jjbarstats_aggregated,
    biomarker = jjbarstats_biomarker
  ),
  path = here("data", "jjbarstats_test.xlsx")
)
jmvReadWrite::write_omv(jjbarstats_test, here("data", "jjbarstats_test.omv"))

# 2. Individual datasets
save(jjbarstats_diagnostic, file = here("data", "jjbarstats_diagnostic.rda"))
save(jjbarstats_paired, file = here("data", "jjbarstats_paired.rda"))
save(jjbarstats_aggregated, file = here("data", "jjbarstats_aggregated.rda"))
save(jjbarstats_biomarker, file = here("data", "jjbarstats_biomarker.rda"))


# ═══════════════════════════════════════════════════════════
# Generate Summary Documentation
# ═══════════════════════════════════════════════════════════

summary_text <- paste0("
═══════════════════════════════════════════════════════════
JJBARSTATS TEST DATA SUMMARY
═══════════════════════════════════════════════════════════

Dataset: jjbarstats_test (Categorical vs Categorical Bar Charts)
Generated: ", Sys.Date(), "
Seed: 42

DIMENSIONS
----------
Main Dataset (jjbarstats_test):
  Observations: ", n, "
  Variables: ", ncol(jjbarstats_test), "
  Treatment groups: 3 (Placebo, Low/High Dose)
  Missing data: ~3% in biomarker_expression

Diagnostic Test Data (jjbarstats_diagnostic):
  Observations: ", n_diagnostic, "
  2×2 contingency table (Diagnosis × Test Result)
  Sensitivity: 0.85, Specificity: 0.90

Paired/Repeated Measures (jjbarstats_paired):
  Observations: ", n_paired, "
  Paired observations: Baseline → Follow-up
  Treatment arms: 2 (Treatment A, Treatment B)

Aggregated Data (jjbarstats_aggregated):
  Observations: 9 (3×3 table with counts)
  Pre-aggregated contingency table

Biomarker Data (jjbarstats_biomarker):
  Observations: ", n_biomarker, "
  Subtypes: 3, Biomarkers: 3 (HER2, ER, PR)

VARIABLE DESCRIPTIONS
---------------------

Categorical Outcomes (for dep parameter):
  • response [factor, 3 levels]: No Response, Partial Response, Complete Response (ordinal)
  • disease_status [factor, 2 levels]: Disease-Free, Recurrence (binary)
  • diagnosis [factor, 2 levels]: Negative, Positive (diagnostic dataset)
  • her2_status [factor, 3 levels]: Negative, Low, High (biomarker dataset)

Grouping Variables (for group parameter):
  • treatment [factor, 3 levels]: Placebo, Low Dose, High Dose
  • tumor_stage [factor, 4 levels]: Stage I, Stage II, Stage III, Stage IV (ordinal)
  • smoking_status [factor, 2 levels]: Non-smoker, Smoker
  • test_result [factor, 2 levels]: Negative, Positive (diagnostic)
  • subtype [factor, 3 levels]: Subtype A, Subtype B, Subtype C

Split-By Variables (for grvar parameter):
  • sex [factor, 2 levels]: Male, Female
  • age_group [factor, 3 levels]: <50, 50-65, >65
  • comorbidity [factor, 2 levels]: Absent, Present

Counts Variable (for counts parameter):
  • count [numeric]: Number of observations (aggregated dataset)

CLINICAL PRESET SCENARIOS
--------------------------

1. Diagnostic Test (clinicalpreset = 'diagnostic'):
   Use: jjbarstats_diagnostic
   dep = diagnosis (Negative/Positive)
   group = test_result (Negative/Positive)
   → Calculates sensitivity, specificity, PPV, NPV

2. Treatment Response (clinicalpreset = 'treatment'):
   Use: jjbarstats_test
   dep = response (No/Partial/Complete Response)
   group = treatment (Placebo/Low/High Dose)
   → Tests treatment effect on response rates

3. Biomarker Expression (clinicalpreset = 'biomarker'):
   Use: jjbarstats_biomarker
   dep = her2_status (Negative/Low/High)
   group = subtype (Subtype A/B/C)
   → Analyzes biomarker distribution across subtypes

4. Risk Factor Analysis (clinicalpreset = 'riskfactor'):
   Use: jjbarstats_test
   dep = disease_status (Disease-Free/Recurrence)
   group = smoking_status (Non-smoker/Smoker)
   → Assesses risk factor association with outcome

STATISTICAL APPROACHES COVERED
-------------------------------

Parametric (typestatistics = 'parametric'):
  • Pearson's chi-square test
  • Cramér's V effect size
  • Example: Response × Treatment

Nonparametric (typestatistics = 'nonparametric'):
  • Fisher's exact test (for small samples)
  • Example: 2×2 diagnostic tables

Robust (typestatistics = 'robust'):
  • Robust chi-square variants
  • Example: Data with outliers or violations

Bayes (typestatistics = 'bayes'):
  • Bayesian contingency table test
  • Bayes Factor for independence
  • Example: Any categorical comparison

Paired/Repeated (paired = TRUE):
  • McNemar's test for paired data
  • Example: Baseline vs Follow-up status

DATA CHARACTERISTICS
--------------------

Treatment Response Data:
  • Response improves with dose: Placebo < Low < High
  • Complete response: 10% (Placebo) → 40% (High Dose)
  • Realistic dose-response relationship

Diagnostic Test Data:
  • Realistic sensitivity (85%) and specificity (90%)
  • Prevalence: 30% (representative of screening scenario)
  • Balanced for chi-square assumptions

Paired/Repeated Measures:
  • Treatment B shows better conversion (75% vs 60%)
  • Realistic within-subject changes
  • Suitable for McNemar's test

Aggregated Data:
  • Pre-counted contingency table
  • Count variable ranges: 5-45
  • Total n = 225

Biomarker Expression:
  • HER2 expression correlates with subtype
  • Subtype C: 50% High expression
  • Subtype A: 70% Negative
  • Ordinal relationship preserved

EXAMPLE R CODE
--------------

# Load test data
data(jjbarstats_test, package = 'ClinicoPath')

# 1. Basic chi-square test (Treatment × Response)
jjbarstats(
  data = jjbarstats_test,
  dep = 'response',
  group = 'treatment',
  typestatistics = 'parametric'
)

# 2. With pairwise comparisons
jjbarstats(
  data = jjbarstats_test,
  dep = 'response',
  group = 'treatment',
  pairwisecomparisons = TRUE,
  padjustmethod = 'holm'
)

# 3. Split by sex (grouped analysis)
jjbarstats(
  data = jjbarstats_test,
  dep = 'response',
  group = 'treatment',
  grvar = 'sex'
)

# 4. Diagnostic test analysis (2×2 table)
data(jjbarstats_diagnostic)
jjbarstats(
  data = jjbarstats_diagnostic,
  dep = 'diagnosis',
  group = 'test_result',
  clinicalpreset = 'diagnostic',
  typestatistics = 'parametric'
)

# 5. Paired/repeated measures (McNemar's test)
data(jjbarstats_paired)
jjbarstats(
  data = jjbarstats_paired,
  dep = 'baseline_status',
  group = 'followup_status',
  paired = TRUE
)

# 6. Aggregated data with counts
data(jjbarstats_aggregated)
jjbarstats(
  data = jjbarstats_aggregated,
  dep = 'response_category',
  group = 'treatment_group',
  counts = 'count'
)

# 7. Biomarker expression analysis
data(jjbarstats_biomarker)
jjbarstats(
  data = jjbarstats_biomarker,
  dep = 'her2_status',
  group = 'subtype',
  clinicalpreset = 'biomarker'
)

# 8. Risk factor analysis
jjbarstats(
  data = jjbarstats_test,
  dep = 'disease_status',
  group = 'smoking_status',
  clinicalpreset = 'riskfactor',
  proportiontest = TRUE
)

# 9. Bayesian analysis
jjbarstats(
  data = jjbarstats_test,
  dep = 'response',
  group = 'treatment',
  typestatistics = 'bayes',
  bfmessage = TRUE
)

# 10. With proportion test and expected ratios
jjbarstats(
  data = jjbarstats_test,
  dep = 'response',
  group = 'treatment',
  proportiontest = TRUE,
  ratio = '0.333,0.333,0.334'  # Equal proportions
)

FILES GENERATED
---------------
  ✓ data/jjbarstats_test.rda              (Comprehensive dataset)
  ✓ data/jjbarstats_diagnostic.rda        (2×2 diagnostic table)
  ✓ data/jjbarstats_paired.rda            (Paired/repeated measures)
  ✓ data/jjbarstats_aggregated.rda        (With counts)
  ✓ data/jjbarstats_biomarker.rda         (Biomarker expression)
  ✓ data/jjbarstats_test.csv              (CSV format)
  ✓ data/jjbarstats_test.xlsx             (Excel with multiple sheets)
  ✓ data/jjbarstats_test.omv              (Jamovi format)

═══════════════════════════════════════════════════════════

✓ All test data files generated successfully!
✓ Summary saved to: JJBARSTATS_TEST_DATA_SUMMARY.md
")

cat(summary_text)
writeLines(summary_text, here("JJBARSTATS_TEST_DATA_SUMMARY.md"))

cat("\n✓ jjbarstats test data generation complete!\n")
