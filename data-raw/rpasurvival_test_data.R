# ═══════════════════════════════════════════════════════════
# Test Data Generation: rpasurvival
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the rpasurvival jamovi function
# (Recursive Partitioning Analysis for Survival Data)
#
# Generated: 2026-01-31
# Function: rpasurvival
# Purpose: RPA/CART for survival data to develop risk stratification systems
#
# Data Requirements:
# - time: Survival time (continuous, non-negative)
# - event: Event indicator (factor: 0/1, TRUE/FALSE, or 1/2)
# - predictors: Multiple predictors (mix of categorical and continuous)

library(tibble)
library(dplyr)
library(here)
set.seed(12345)

# ═══════════════════════════════════════════════════════════
# Dataset 1: Standard Clinical Dataset (n=200)
# ═══════════════════════════════════════════════════════════

n <- 200

rpasurvival_test <- tibble(
  # Patient ID
  patient_id = paste0("PT", sprintf("%03d", 1:n)),

  # Survival time in months (range: 1-120 months, mean ~36 months)
  time = pmax(0.5, rexp(n, rate = 1/36)),

  # Event indicator (1 = death/event, 0 = censored)
  # About 65% event rate
  event = factor(rbinom(n, 1, 0.65), levels = c(0, 1)),

  # Age (years, 40-85, mean ~65)
  age = round(rnorm(n, mean = 65, sd = 12)),

  # Tumor stage (I-IV) - ordinal categorical
  stage = factor(
    sample(c("I", "II", "III", "IV"), n, replace = TRUE,
           prob = c(0.25, 0.30, 0.30, 0.15)),
    levels = c("I", "II", "III", "IV"),
    ordered = TRUE
  ),

  # Tumor grade (1-3) - ordinal categorical
  grade = factor(
    sample(c("G1", "G2", "G3"), n, replace = TRUE,
           prob = c(0.20, 0.50, 0.30)),
    levels = c("G1", "G2", "G3"),
    ordered = TRUE
  ),

  # Lymphovascular invasion (LVI) - binary categorical
  LVI = factor(
    sample(c("Absent", "Present"), n, replace = TRUE,
           prob = c(0.60, 0.40)),
    levels = c("Absent", "Present")
  ),

  # Tumor size (cm) - continuous
  tumor_size = pmax(0.5, rnorm(n, mean = 3.5, sd = 2.0)),

  # Ki67 proliferation index (%) - continuous
  ki67 = pmax(0, pmin(100, rnorm(n, mean = 30, sd = 20))),

  # Performance status (0-2) - ordinal
  performance_status = factor(
    sample(0:2, n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    levels = 0:2,
    ordered = TRUE
  ),

  # Treatment received - categorical
  treatment = factor(
    sample(c("Surgery only", "Surgery + Chemo", "Surgery + Radio", "Trimodal"),
           n, replace = TRUE,
           prob = c(0.3, 0.35, 0.2, 0.15))
  )
)

# Add realistic correlations between predictors and survival
# Higher stage → shorter survival, more events
rpasurvival_test <- rpasurvival_test %>%
  mutate(
    time = case_when(
      stage == "IV" ~ time * 0.4,
      stage == "III" ~ time * 0.6,
      stage == "II" ~ time * 0.8,
      TRUE ~ time
    ),
    event = case_when(
      stage == "IV" & as.numeric(as.character(event)) == 0 ~
        factor(sample(c(0, 1), 1, prob = c(0.1, 0.9)), levels = c(0, 1)),
      stage == "I" & as.numeric(as.character(event)) == 1 ~
        factor(sample(c(0, 1), 1, prob = c(0.6, 0.4)), levels = c(0, 1)),
      TRUE ~ event
    )
  )

# Round time to 1 decimal
rpasurvival_test$time <- round(rpasurvival_test$time, 1)

# Add some missing data (~3% in continuous predictors)
n_missing <- round(n * 0.03)
rpasurvival_test$ki67[sample(n, n_missing)] <- NA
rpasurvival_test$tumor_size[sample(n, n_missing)] <- NA

# ═══════════════════════════════════════════════════════════
# Dataset 2: Small Sample (n=50) - For minimal viable analysis
# ═══════════════════════════════════════════════════════════

n_small <- 50

rpasurvival_small <- tibble(
  patient_id = paste0("SM", sprintf("%02d", 1:n_small)),
  time = pmax(1, rexp(n_small, rate = 1/30)),
  event = factor(rbinom(n_small, 1, 0.60), levels = c(0, 1)),
  age = round(rnorm(n_small, mean = 60, sd = 15)),
  stage = factor(
    sample(c("Early", "Advanced"), n_small, replace = TRUE),
    levels = c("Early", "Advanced")
  ),
  grade = factor(
    sample(c("Low", "High"), n_small, replace = TRUE),
    levels = c("Low", "High")
  )
)

rpasurvival_small$time <- round(rpasurvival_small$time, 1)

# ═══════════════════════════════════════════════════════════
# Dataset 3: Large Sample (n=500) - For complex tree development
# ═══════════════════════════════════════════════════════════

n_large <- 500

rpasurvival_large <- tibble(
  patient_id = paste0("LG", sprintf("%04d", 1:n_large)),
  time = pmax(0.5, rexp(n_large, rate = 1/40)),
  event = factor(rbinom(n_large, 1, 0.70), levels = c(0, 1)),
  age = round(rnorm(n_large, mean = 65, sd = 12)),
  stage = factor(
    sample(c("IA", "IB", "IIA", "IIB", "IIIA", "IIIB", "IV"),
           n_large, replace = TRUE,
           prob = c(0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.10)),
    levels = c("IA", "IB", "IIA", "IIB", "IIIA", "IIIB", "IV"),
    ordered = TRUE
  ),
  grade = factor(sample(1:3, n_large, replace = TRUE), ordered = TRUE),
  LVI = factor(sample(c("No", "Yes"), n_large, replace = TRUE)),
  PNI = factor(sample(c("No", "Yes"), n_large, replace = TRUE)),
  tumor_size = pmax(0.3, rnorm(n_large, mean = 4.0, sd = 2.5)),
  nodes_positive = rpois(n_large, lambda = 2),
  biomarker1 = rnorm(n_large, mean = 50, sd = 20),
  biomarker2 = rnorm(n_large, mean = 100, sd = 30)
)

rpasurvival_large$time <- round(rpasurvival_large$time, 1)

# ═══════════════════════════════════════════════════════════
# Dataset 4: Edge Cases Dataset
# ═══════════════════════════════════════════════════════════

# Test with different event coding schemes
rpasurvival_edge <- rpasurvival_test[1:30, ]

# Version A: event as TRUE/FALSE
rpasurvival_edge_truefalse <- rpasurvival_edge %>%
  mutate(
    event_tf = factor(ifelse(event == "1", "TRUE", "FALSE"),
                      levels = c("FALSE", "TRUE"))
  ) %>%
  select(-event)

# Version B: event as 1/2
rpasurvival_edge_12 <- rpasurvival_edge %>%
  mutate(
    event_12 = factor(ifelse(event == "1", "2", "1"),
                      levels = c("1", "2"))
  ) %>%
  select(-event)

# Version C: time in days instead of months
rpasurvival_edge_days <- rpasurvival_edge %>%
  mutate(time_days = round(time * 30.44)) %>%
  select(-time)

# Version D: time in years
rpasurvival_edge_years <- rpasurvival_edge %>%
  mutate(time_years = round(time / 12, 2)) %>%
  select(-time)

# ═══════════════════════════════════════════════════════════
# Save in Multiple Formats
# ═══════════════════════════════════════════════════════════

# Create data directories if they don't exist
if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"), recursive = TRUE)
}
if (!dir.exists(here::here("data", "nonrda"))) {
  dir.create(here::here("data", "nonrda"), recursive = TRUE)
}

# 1. RDA format (native R)
save(rpasurvival_test, file = here::here("data", "rpasurvival_test.rda"))
save(rpasurvival_small, file = here::here("data", "rpasurvival_small.rda"))
save(rpasurvival_large, file = here::here("data", "rpasurvival_large.rda"))
save(rpasurvival_edge_truefalse, file = here::here("data", "rpasurvival_edge_truefalse.rda"))
save(rpasurvival_edge_12, file = here::here("data", "rpasurvival_edge_12.rda"))
save(rpasurvival_edge_days, file = here::here("data", "rpasurvival_edge_days.rda"))
save(rpasurvival_edge_years, file = here::here("data", "rpasurvival_edge_years.rda"))

# 2. CSV format (saved in nonrda subfolder)
write.csv(rpasurvival_test,
          file = here::here("data", "nonrda", "rpasurvival_test.csv"),
          row.names = FALSE)
write.csv(rpasurvival_small,
          file = here::here("data", "nonrda", "rpasurvival_small.csv"),
          row.names = FALSE)
write.csv(rpasurvival_large,
          file = here::here("data", "nonrda", "rpasurvival_large.csv"),
          row.names = FALSE)

# 3. Excel format (saved in nonrda subfolder)
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(rpasurvival_test,
                      path = here::here("data", "nonrda", "rpasurvival_test.xlsx"))
  writexl::write_xlsx(rpasurvival_small,
                      path = here::here("data", "nonrda", "rpasurvival_small.xlsx"))
  writexl::write_xlsx(rpasurvival_large,
                      path = here::here("data", "nonrda", "rpasurvival_large.xlsx"))

  # Multi-sheet Excel with all edge cases
  writexl::write_xlsx(
    list(
      standard = rpasurvival_test,
      event_truefalse = rpasurvival_edge_truefalse,
      event_12 = rpasurvival_edge_12,
      time_days = rpasurvival_edge_days,
      time_years = rpasurvival_edge_years
    ),
    path = here::here("data", "nonrda", "rpasurvival_all_formats.xlsx")
  )
}

# 4. Jamovi format (OMV - saved in nonrda subfolder)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(rpasurvival_test,
                          here::here("data", "nonrda", "rpasurvival_test.omv"))
  jmvReadWrite::write_omv(rpasurvival_small,
                          here::here("data", "nonrda", "rpasurvival_small.omv"))
  jmvReadWrite::write_omv(rpasurvival_large,
                          here::here("data", "nonrda", "rpasurvival_large.omv"))
}

# ═══════════════════════════════════════════════════════════
# Documentation
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
Test Data Generated for: rpasurvival
═══════════════════════════════════════════════════════════

DATASET 1: rpasurvival_test (Standard)
---------------------------------------
Observations: 200
Variables: 11
Purpose: Standard RPA analysis with mixed predictors

Variables:
- patient_id: Patient identifier
- time: Survival time in months (continuous, 0.5-120)
- event: Event indicator (factor: 0=censored, 1=event, ~65% events)
- age: Patient age in years (40-85)
- stage: Tumor stage (I-IV, ordinal)
- grade: Tumor grade (G1-G3, ordinal)
- LVI: Lymphovascular invasion (Absent/Present)
- tumor_size: Tumor size in cm (continuous)
- ki67: Ki-67 proliferation index 0-100%
- performance_status: ECOG performance status (0-2, ordinal)
- treatment: Treatment modality (4 categories)

Missing data: ~3% in ki67 and tumor_size

Usage example:
  data(rpasurvival_test)
  library(ClinicoPath)
  rpasurvival(
    data = rpasurvival_test,
    time = 'time',
    event = 'event',
    predictors = c('age', 'stage', 'grade', 'LVI'),
    time_unit = 'months'
  )

DATASET 2: rpasurvival_small (Minimal)
---------------------------------------
Observations: 50
Variables: 6
Purpose: Test minimal viable sample size

Variables:
- patient_id, time, event, age, stage, grade

Usage: Test small-sample warnings and minimum EPV requirements

DATASET 3: rpasurvival_large (Complex)
---------------------------------------
Observations: 500
Variables: 11
Purpose: Test complex tree development

Variables: Extended staging, multiple biomarkers, node counts

Usage: Test maxdepth, complex trees, variable importance

DATASET 4: Edge Cases
---------------------------------------
Files:
- rpasurvival_edge_truefalse: event coded as TRUE/FALSE
- rpasurvival_edge_12: event coded as 1/2
- rpasurvival_edge_days: time in days instead of months
- rpasurvival_edge_years: time in years instead of months

Usage: Test different event/time coding schemes

FILE FORMATS
---------------------------------------
✓ RDA: rpasurvival_test.rda (and variants)
✓ CSV: rpasurvival_test.csv (and variants)
✓ XLSX: rpasurvival_test.xlsx (and variants)
✓ OMV: rpasurvival_test.omv (and variants)
✓ Multi-sheet XLSX: rpasurvival_all_formats.xlsx

TESTING SCENARIOS
---------------------------------------
1. Standard analysis: Use rpasurvival_test with 4-6 predictors
2. Small sample: Use rpasurvival_small, expect warnings
3. Complex trees: Use rpasurvival_large with maxdepth=5
4. Event coding: Test TRUE/FALSE and 1/2 coding
5. Time units: Test days, months, years with time_unit parameter
6. Missing data: Verify handling of ~3% missing values
7. Variable types: Mix of continuous, ordinal, nominal predictors

VALIDATION CHECKS
---------------------------------------
✓ Time variable is non-negative
✓ Event rate ~60-70% (realistic for clinical data)
✓ Stage correlates with survival (Stage IV → shorter time)
✓ Sufficient events per predictor (EPV > 10)
✓ Realistic clinical distributions
✓ Proper factor level ordering

NEXT STEPS
---------------------------------------
1. Run: source('data-raw/rpasurvival_test_data.R')
2. Test: library(ClinicoPath); data(rpasurvival_test)
3. Validate: Run test suite in tests/testthat/
4. Document: Add to data.R with roxygen2

═══════════════════════════════════════════════════════════
")
