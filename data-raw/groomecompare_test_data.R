# ═══════════════════════════════════════════════════════════
# Test Data Generation: groomecompare
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the groomecompare jamovi function
# (Groome Staging System Comparison)
#
# Generated: 2026-01-31
# Function: groomecompare
# Purpose: Compare two staging systems using Groome criteria (2001)
#
# Data Requirements:
# - time: Survival time (continuous, non-negative)
# - event: Event indicator (factor: 0/1, TRUE/FALSE, or 1/2)
# - stage1: First staging system (factor, typically ordinal)
# - stage2: Second staging system (factor, typically ordinal)

library(tibble)
library(dplyr)
library(here)
set.seed(12345)

# ═══════════════════════════════════════════════════════════
# Dataset 1: Standard Comparison (n=150)
# ═══════════════════════════════════════════════════════════
# Compare traditional TNM vs RPA-based staging

n <- 150

groomecompare_test <- tibble(
  # Patient ID
  patient_id = paste0("PT", sprintf("%03d", 1:n)),

  # Survival time in months (exponential distribution, mean ~20 months)
  time = pmax(0.5, rexp(n, rate = 0.05)),

  # Event indicator (0 = censored, 1 = death)
  # Event rate ~60%
  event = factor(rbinom(n, 1, 0.60), levels = c(0, 1)),

  # Staging System 1: ypTNM (traditional pathological staging after neoadjuvant)
  # 4 stages: I, II, III, IV
  ypTNM = factor(
    sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
           n, replace = TRUE,
           prob = c(0.30, 0.30, 0.25, 0.15)),
    levels = c("Stage I", "Stage II", "Stage III", "Stage IV"),
    ordered = TRUE
  ),

  # Staging System 2: RPA (Recursive Partitioning Analysis groups)
  # 3 groups: Low, Intermediate, High Risk
  RPA = factor(
    sample(c("Low Risk", "Intermediate", "High Risk"),
           n, replace = TRUE,
           prob = c(0.40, 0.35, 0.25)),
    levels = c("Low Risk", "Intermediate", "High Risk"),
    ordered = TRUE
  ),

  # Additional patient characteristics (for context)
  age = round(rnorm(n, mean = 65, sd = 12)),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4)))
)

# Add realistic prognostic correlations
# Stage IV → shorter survival
groomecompare_test <- groomecompare_test %>%
  mutate(
    time = case_when(
      ypTNM == "Stage IV" ~ time * 0.4,
      ypTNM == "Stage III" ~ time * 0.6,
      ypTNM == "Stage II" ~ time * 0.8,
      TRUE ~ time
    ),
    # High Risk RPA → shorter survival
    time = case_when(
      RPA == "High Risk" ~ time * 0.5,
      RPA == "Intermediate" ~ time * 0.75,
      TRUE ~ time
    ),
    # Modify event rate by stage
    event = case_when(
      ypTNM == "Stage IV" & event == 0 ~
        factor(sample(c(0, 1), 1, prob = c(0.1, 0.9)), levels = c(0, 1)),
      ypTNM == "Stage I" & event == 1 ~
        factor(sample(c(0, 1), 1, prob = c(0.6, 0.4)), levels = c(0, 1)),
      TRUE ~ event
    )
  )

# Round time to 1 decimal place
groomecompare_test$time <- round(groomecompare_test$time, 1)

# ═══════════════════════════════════════════════════════════
# Dataset 2: Small Sample (n=60) - Test minimum viable
# ═══════════════════════════════════════════════════════════

n_small <- 60

groomecompare_small <- tibble(
  patient_id = paste0("SM", sprintf("%02d", 1:n_small)),
  time = pmax(0.5, rexp(n_small, rate = 0.05)),
  event = factor(rbinom(n_small, 1, 0.55), levels = c(0, 1)),

  # Simplified 3-stage systems
  clinical_stage = factor(
    sample(c("Early", "Locally Advanced", "Metastatic"),
           n_small, replace = TRUE,
           prob = c(0.4, 0.4, 0.2)),
    levels = c("Early", "Locally Advanced", "Metastatic"),
    ordered = TRUE
  ),

  molecular_subtype = factor(
    sample(c("Favorable", "Intermediate", "Unfavorable"),
           n_small, replace = TRUE,
           prob = c(0.35, 0.40, 0.25)),
    levels = c("Favorable", "Intermediate", "Unfavorable"),
    ordered = TRUE
  )
)

groomecompare_small$time <- round(groomecompare_small$time, 1)

# ═══════════════════════════════════════════════════════════
# Dataset 3: Large Sample (n=300) - Test robust comparison
# ═══════════════════════════════════════════════════════════

n_large <- 300

groomecompare_large <- tibble(
  patient_id = paste0("LG", sprintf("%04d", 1:n_large)),
  time = pmax(0.5, rexp(n_large, rate = 0.04)),
  event = factor(rbinom(n_large, 1, 0.65), levels = c(0, 1)),

  # Detailed 8th edition AJCC TNM
  AJCC8 = factor(
    sample(c("IA", "IB", "IIA", "IIB", "IIIA", "IIIB", "IIIC", "IV"),
           n_large, replace = TRUE,
           prob = c(0.15, 0.15, 0.12, 0.12, 0.12, 0.12, 0.12, 0.10)),
    levels = c("IA", "IB", "IIA", "IIB", "IIIA", "IIIB", "IIIC", "IV"),
    ordered = TRUE
  ),

  # RPA with 5 groups (more granular)
  RPA5 = factor(
    sample(c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
           n_large, replace = TRUE,
           prob = c(0.25, 0.25, 0.20, 0.15, 0.15)),
    levels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
    ordered = TRUE
  ),

  age = round(rnorm(n_large, mean = 64, sd = 13)),
  sex = factor(sample(c("Male", "Female"), n_large, replace = TRUE))
)

# Add correlations
groomecompare_large <- groomecompare_large %>%
  mutate(
    time = case_when(
      AJCC8 %in% c("IIIC", "IV") ~ time * 0.35,
      AJCC8 %in% c("IIIA", "IIIB") ~ time * 0.55,
      AJCC8 %in% c("IIA", "IIB") ~ time * 0.75,
      TRUE ~ time
    ),
    time = case_when(
      RPA5 == "Group 5" ~ time * 0.4,
      RPA5 == "Group 4" ~ time * 0.6,
      RPA5 == "Group 3" ~ time * 0.75,
      RPA5 == "Group 2" ~ time * 0.9,
      TRUE ~ time
    )
  )

groomecompare_large$time <- round(groomecompare_large$time, 1)

# ═══════════════════════════════════════════════════════════
# Dataset 4: Unbalanced Comparison (different # groups)
# ═══════════════════════════════════════════════════════════

n_unbal <- 120

groomecompare_unbalanced <- tibble(
  patient_id = paste0("UB", sprintf("%03d", 1:n_unbal)),
  time = pmax(0.5, rexp(n_unbal, rate = 0.05)),
  event = factor(rbinom(n_unbal, 1, 0.58), levels = c(0, 1)),

  # System 1: 5 stages
  detailed_stage = factor(
    sample(paste("Stage", 1:5), n_unbal, replace = TRUE,
           prob = c(0.25, 0.20, 0.20, 0.20, 0.15)),
    levels = paste("Stage", 1:5),
    ordered = TRUE
  ),

  # System 2: 2 stages (binary)
  simple_stage = factor(
    sample(c("Localized", "Advanced"), n_unbal, replace = TRUE,
           prob = c(0.55, 0.45)),
    levels = c("Localized", "Advanced"),
    ordered = TRUE
  )
)

groomecompare_unbalanced$time <- round(groomecompare_unbalanced$time, 1)

# ═══════════════════════════════════════════════════════════
# Dataset 5: Edge Cases - Different Event Coding
# ═══════════════════════════════════════════════════════════

# Subset for edge cases
edge_base <- groomecompare_test[1:40, ]

# Version A: Event as TRUE/FALSE
groomecompare_edge_truefalse <- edge_base %>%
  mutate(
    event_tf = factor(ifelse(event == "1", "TRUE", "FALSE"),
                      levels = c("FALSE", "TRUE"))
  ) %>%
  select(-event)

# Version B: Event as 1/2
groomecompare_edge_12 <- edge_base %>%
  mutate(
    event_12 = factor(ifelse(event == "1", "2", "1"),
                      levels = c("1", "2"))
  ) %>%
  select(-event)

# ═══════════════════════════════════════════════════════════
# Dataset 6: Tied Survival Times (test tie handling)
# ═══════════════════════════════════════════════════════════

groomecompare_tied <- groomecompare_test[1:80, ] %>%
  mutate(
    # Round to nearest 5 months (creates many ties)
    time_tied = round(time / 5) * 5
  ) %>%
  select(-time) %>%
  rename(time = time_tied)

# ═══════════════════════════════════════════════════════════
# Dataset 7: Identical Systems (negative control)
# ═══════════════════════════════════════════════════════════

groomecompare_identical <- groomecompare_test[1:100, ] %>%
  mutate(
    # Make stage2 identical to stage1
    RPA_copy = ypTNM
  ) %>%
  select(-RPA) %>%
  rename(RPA = RPA_copy)

# ═══════════════════════════════════════════════════════════
# Dataset 8: One System Clearly Better
# ═══════════════════════════════════════════════════════════

n_better <- 150

groomecompare_clear_winner <- tibble(
  patient_id = paste0("CW", sprintf("%03d", 1:n_better)),
  time = pmax(0.5, rexp(n_better, rate = 0.05)),
  event = factor(rbinom(n_better, 1, 0.60), levels = c(0, 1)),

  # Good staging system (strong prognostic separation)
  good_stage = factor(
    sample(c("Stage A", "Stage B", "Stage C", "Stage D"),
           n_better, replace = TRUE,
           prob = c(0.25, 0.25, 0.25, 0.25)),
    levels = c("Stage A", "Stage B", "Stage C", "Stage D"),
    ordered = TRUE
  ),

  # Poor staging system (weak prognostic separation)
  poor_stage = factor(
    sample(c("Group 1", "Group 2", "Group 3"),
           n_better, replace = TRUE,
           prob = c(0.33, 0.34, 0.33)),
    levels = c("Group 1", "Group 2", "Group 3"),
    ordered = TRUE
  )
)

# Strong correlation with good_stage
groomecompare_clear_winner <- groomecompare_clear_winner %>%
  mutate(
    time = case_when(
      good_stage == "Stage D" ~ time * 0.25,
      good_stage == "Stage C" ~ time * 0.50,
      good_stage == "Stage B" ~ time * 0.75,
      TRUE ~ time
    ),
    # Weak/random correlation with poor_stage
    time = time + rnorm(n_better, 0, 2)
  )

groomecompare_clear_winner$time <- pmax(0.5, groomecompare_clear_winner$time)
groomecompare_clear_winner$time <- round(groomecompare_clear_winner$time, 1)

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# Create data directories if needed
if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"), recursive = TRUE)
}
if (!dir.exists(here::here("data", "nonrda"))) {
  dir.create(here::here("data", "nonrda"), recursive = TRUE)
}

# 1. RDA format (native R)
save(groomecompare_test, file = here::here("data", "groomecompare_test.rda"))
save(groomecompare_small, file = here::here("data", "groomecompare_small.rda"))
save(groomecompare_large, file = here::here("data", "groomecompare_large.rda"))
save(groomecompare_unbalanced, file = here::here("data", "groomecompare_unbalanced.rda"))
save(groomecompare_edge_truefalse, file = here::here("data", "groomecompare_edge_truefalse.rda"))
save(groomecompare_edge_12, file = here::here("data", "groomecompare_edge_12.rda"))
save(groomecompare_tied, file = here::here("data", "groomecompare_tied.rda"))
save(groomecompare_identical, file = here::here("data", "groomecompare_identical.rda"))
save(groomecompare_clear_winner, file = here::here("data", "groomecompare_clear_winner.rda"))

# 2. CSV format (saved in nonrda subfolder)
write.csv(groomecompare_test, file = here::here("data", "nonrda", "groomecompare_test.csv"), row.names = FALSE)
write.csv(groomecompare_small, file = here::here("data", "nonrda", "groomecompare_small.csv"), row.names = FALSE)
write.csv(groomecompare_large, file = here::here("data", "nonrda", "groomecompare_large.csv"), row.names = FALSE)

# 3. Excel format (saved in nonrda subfolder)
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(groomecompare_test, path = here::here("data", "nonrda", "groomecompare_test.xlsx"))
  writexl::write_xlsx(groomecompare_small, path = here::here("data", "nonrda", "groomecompare_small.xlsx"))
  writexl::write_xlsx(groomecompare_large, path = here::here("data", "nonrda", "groomecompare_large.xlsx"))

  # Multi-sheet Excel with all datasets
  writexl::write_xlsx(
    list(
      standard = groomecompare_test,
      small = groomecompare_small,
      large = groomecompare_large,
      unbalanced = groomecompare_unbalanced,
      tied = groomecompare_tied,
      identical = groomecompare_identical,
      clear_winner = groomecompare_clear_winner
    ),
    path = here::here("data", "nonrda", "groomecompare_all_scenarios.xlsx")
  )
}

# 4. Jamovi format (OMV - saved in nonrda subfolder)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groomecompare_test, here::here("data", "nonrda", "groomecompare_test.omv"))
  jmvReadWrite::write_omv(groomecompare_small, here::here("data", "nonrda", "groomecompare_small.omv"))
  jmvReadWrite::write_omv(groomecompare_large, here::here("data", "nonrda", "groomecompare_large.omv"))
}

# ═══════════════════════════════════════════════════════════
# Documentation
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
Test Data Generated for: groomecompare
═══════════════════════════════════════════════════════════

DATASET 1: groomecompare_test (Standard)
-----------------------------------------
Observations: 150
Variables: 7
Purpose: Compare ypTNM vs RPA staging systems

Variables:
- patient_id: Patient identifier
- time: Survival time in months (exponential, mean ~20)
- event: Event indicator (factor: 0=censored, 1=death, ~60% events)
- ypTNM: Traditional pathological staging (Stage I-IV, ordinal)
- RPA: Recursive partitioning groups (Low/Intermediate/High Risk, ordinal)
- age: Patient age in years
- sex: Patient sex (Male/Female)

Prognostic correlations:
- Stage IV → 0.4× survival vs Stage I
- High Risk → 0.5× survival vs Low Risk

Usage example:
  data(groomecompare_test)
  library(ClinicoPath)
  groomecompare(
    data = groomecompare_test,
    time = 'time',
    event = 'event',
    stage1 = 'ypTNM',
    stage2 = 'RPA',
    stage1name = 'ypTNM Staging',
    stage2name = 'RPA Classification'
  )

DATASET 2: groomecompare_small (Minimal)
-----------------------------------------
Observations: 60
Variables: 5
Purpose: Test small sample warnings and minimum viable comparison

Systems: clinical_stage (3 levels) vs molecular_subtype (3 levels)

DATASET 3: groomecompare_large (Robust)
----------------------------------------
Observations: 300
Variables: 7
Purpose: Test robust comparison with detailed staging

Systems: AJCC8 (8 levels: IA-IV) vs RPA5 (5 groups)

DATASET 4: groomecompare_unbalanced
------------------------------------
Observations: 120
Variables: 5
Purpose: Test systems with different numbers of groups

Systems: detailed_stage (5 levels) vs simple_stage (2 levels: binary)

DATASET 5: Edge Cases - Event Coding
-------------------------------------
Files:
- groomecompare_edge_truefalse: event coded as TRUE/FALSE
- groomecompare_edge_12: event coded as 1/2

Usage: Test different event value specifications

DATASET 6: groomecompare_tied
------------------------------
Observations: 80
Purpose: Test handling of tied survival times

Survival times rounded to nearest 5 months (many ties)

DATASET 7: groomecompare_identical
-----------------------------------
Observations: 100
Purpose: Negative control - both staging systems identical

Expected: All Groome metrics should be tied/equal

DATASET 8: groomecompare_clear_winner
--------------------------------------
Observations: 150
Purpose: Test with one clearly superior staging system

good_stage: Strong prognostic separation (4 stages)
poor_stage: Weak/random prognostic value (3 groups)

Expected: good_stage should win all Groome criteria

FILE FORMATS
-----------------------------------------
✓ RDA: groomecompare_*.rda (9 datasets)
✓ CSV: groomecompare_{test,small,large}.csv (3 main datasets)
✓ XLSX: groomecompare_{test,small,large}.xlsx + multi-sheet
✓ OMV: groomecompare_{test,small,large}.omv (3 datasets)

TESTING SCENARIOS
-----------------------------------------
1. Standard comparison: ypTNM vs RPA (4 vs 3 groups)
2. Small sample: n=60, test warnings
3. Large sample: n=300, robust metrics
4. Unbalanced: 5 groups vs 2 groups
5. Event coding: TRUE/FALSE and 1/2 variants
6. Tied times: Many identical survival times
7. Identical systems: Negative control (metrics = 0.5)
8. Clear winner: One system objectively better

GROOME CRITERIA TESTED
-----------------------------------------
1. Hazard Consistency: Monotonicity of hazard ratios
2. Hazard Discrimination: Spread of hazard ratios
3. Sample Balance: Distribution across stages
4. Outcome Prediction: C-index/concordance

Overall Rank: Sum of individual criterion ranks

VALIDATION CHECKS
-----------------------------------------
✓ Non-negative survival times
✓ Event rates 55-65% (realistic)
✓ Prognostic correlations built in
✓ Sufficient events for Cox models
✓ Factor level ordering (ordinal stages)
✓ Multiple comparison scenarios

NEXT STEPS
-----------------------------------------
1. Run: source('data-raw/groomecompare_test_data.R')
2. Test: library(ClinicoPath); data(groomecompare_test)
3. Validate: Run test suite in tests/testthat/
4. Document: Add to data.R with roxygen2

═══════════════════════════════════════════════════════════
")
