# ═══════════════════════════════════════════════════════════
# Test Data Generation: linechart
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the linechart jamovi function
#
# Generated: 2026-01-05
# Seed: 42
# Purpose: Time series and trend visualization testing

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# ═══════════════════════════════════════════════════════════
# Dataset 1: linechart_simple (n=100)
# Basic time series with single line
# ═══════════════════════════════════════════════════════════

n_simple <- 100
time_points <- 1:n_simple

linechart_simple <- tibble(
  # Time point (sequential)
  time_point = time_points,

  # Value with linear trend and noise
  value = 50 + 0.5 * time_point + rnorm(n_simple, mean = 0, sd = 5),

  # Alternative value with different pattern
  temperature = 20 + 3 * sin(time_point / 10) + rnorm(n_simple, mean = 0, sd = 1)
)

# ═══════════════════════════════════════════════════════════
# Dataset 2: linechart_grouped (n=150)
# Grouped time series with three treatment groups
# ═══════════════════════════════════════════════════════════

n_per_group <- 50
time_seq <- 1:n_per_group

linechart_grouped <- tibble(
  # Time point
  time_point = rep(time_seq, 3),

  # Treatment groups
  treatment = rep(c("Control", "Treatment A", "Treatment B"), each = n_per_group),

  # Lab value (different trends by group)
  lab_value = c(
    # Control: slight increase
    100 + 0.3 * time_seq + rnorm(n_per_group, 0, 8),
    # Treatment A: moderate decrease
    100 - 0.8 * time_seq + rnorm(n_per_group, 0, 8),
    # Treatment B: strong decrease
    100 - 1.5 * time_seq + rnorm(n_per_group, 0, 8)
  ),

  # Patient ID
  patient_id = rep(1:n_per_group, 3)
)

# ═══════════════════════════════════════════════════════════
# Dataset 3: linechart_clinical (n=180)
# Clinical measurements over time (weeks)
# ═══════════════════════════════════════════════════════════

n_weeks <- 12
n_patients <- 15

linechart_clinical <- tibble(
  # Week number
  week = rep(0:n_weeks, n_patients),

  # Patient group
  disease_stage = rep(sample(c("Early", "Intermediate", "Advanced"), n_patients, replace = TRUE), each = n_weeks + 1),

  # Tumor marker level
  tumor_marker = case_when(
    disease_stage == "Early" ~ 50 + rnorm(n_patients * (n_weeks + 1), 0, 10) - 0.5 * week,
    disease_stage == "Intermediate" ~ 80 + rnorm(n_patients * (n_weeks + 1), 0, 12) + 0.2 * week,
    disease_stage == "Advanced" ~ 120 + rnorm(n_patients * (n_weeks + 1), 0, 15) + 1.0 * week
  ),

  # White blood cell count
  wbc_count = 7.5 + rnorm(n_patients * (n_weeks + 1), 0, 1.5) - 0.1 * week,

  # Patient identifier
  patient = rep(1:n_patients, each = n_weeks + 1)
)

# ═══════════════════════════════════════════════════════════
# Dataset 4: linechart_short (n=20)
# Short time series with few time points
# ═══════════════════════════════════════════════════════════

n_short <- 5
n_subjects_short <- 4

linechart_short <- tibble(
  # Time point (0, 3, 6, 9, 12 months)
  month = rep(c(0, 3, 6, 9, 12), n_subjects_short),

  # Response group
  response_group = rep(c("Responder", "Non-responder"), each = n_short * 2),

  # Measurement
  measurement = c(
    # Responders
    100 + c(0, -15, -30, -40, -45) + rnorm(n_short * 2, 0, 5),
    # Non-responders
    100 + c(0, -2, 5, 10, 15) + rnorm(n_short * 2, 0, 5)
  ),

  # Subject ID
  subject = rep(1:n_subjects_short, each = n_short)
)

# ═══════════════════════════════════════════════════════════
# Dataset 5: linechart_long (n=365)
# Long time series (daily measurements for a year)
# ═══════════════════════════════════════════════════════════

n_days <- 365

linechart_long <- tibble(
  # Day of year
  day = 1:n_days,

  # Seasonal pattern (temperature-like)
  seasonal_value = 15 + 10 * sin((day - 80) * 2 * pi / 365) + rnorm(n_days, 0, 2),

  # Cyclic pattern with trend
  cyclic_value = 50 + 0.05 * day + 5 * sin(day * 2 * pi / 30) + rnorm(n_days, 0, 3),

  # Month (for grouping)
  month = ceiling(day / 30.4167)
)

# ═══════════════════════════════════════════════════════════
# Dataset 6: linechart_irregular (n=120)
# Irregular time intervals
# ═══════════════════════════════════════════════════════════

n_irregular <- 120

# Generate irregular time points
irregular_times <- sort(c(
  runif(40, 0, 10),
  runif(40, 10, 30),
  runif(40, 30, 100)
))

linechart_irregular <- tibble(
  # Irregular time points
  time = irregular_times,

  # Observation type
  obs_type = sample(c("Type A", "Type B", "Type C"), n_irregular, replace = TRUE),

  # Value with exponential decay
  value = 100 * exp(-irregular_times / 50) + rnorm(n_irregular, 0, 5),

  # Observation ID
  obs_id = 1:n_irregular
)

# ═══════════════════════════════════════════════════════════
# Dataset 7: linechart_multiple (n=200)
# Multiple measurements per time point
# ═══════════════════════════════════════════════════════════

n_time_points <- 10
n_replicates <- 20

linechart_multiple <- tibble(
  # Time point
  visit = rep(1:n_time_points, n_replicates),

  # Intervention group
  intervention = rep(sample(c("Placebo", "Drug A", "Drug B", "Drug C"),
                           n_replicates, replace = TRUE),
                    each = n_time_points),

  # Blood pressure
  systolic_bp = case_when(
    intervention == "Placebo" ~ 140 - 0.5 * visit + rnorm(n_time_points * n_replicates, 0, 10),
    intervention == "Drug A" ~ 140 - 2.0 * visit + rnorm(n_time_points * n_replicates, 0, 10),
    intervention == "Drug B" ~ 140 - 3.5 * visit + rnorm(n_time_points * n_replicates, 0, 10),
    intervention == "Drug C" ~ 140 - 2.5 * visit + rnorm(n_time_points * n_replicates, 0, 10)
  ),

  # Cholesterol level
  cholesterol = 220 - 1.5 * visit + rnorm(n_time_points * n_replicates, 0, 15),

  # Patient ID
  patient_id = rep(1:n_replicates, each = n_time_points)
)

# ═══════════════════════════════════════════════════════════
# Dataset 8: linechart_patterns (n=150)
# Different trend patterns for testing
# ═══════════════════════════════════════════════════════════

n_pattern <- 50

linechart_patterns <- tibble(
  # Time index
  time_index = rep(1:n_pattern, 3),

  # Pattern type
  pattern_type = rep(c("Increasing", "Decreasing", "Stable"), each = n_pattern),

  # Value with different patterns
  value = c(
    # Increasing (exponential growth)
    20 + 0.5 * (1:n_pattern)^1.2 + rnorm(n_pattern, 0, 3),
    # Decreasing (logarithmic decay)
    100 - 15 * log(1:n_pattern + 1) + rnorm(n_pattern, 0, 3),
    # Stable (fluctuating around mean)
    50 + rnorm(n_pattern, 0, 5)
  ),

  # Secondary measure
  variance = abs(rnorm(n_pattern * 3, mean = 10, sd = 3))
)

# ═══════════════════════════════════════════════════════════
# Add some missing data (~3%)
# ═══════════════════════════════════════════════════════════

linechart_clinical$tumor_marker[sample(nrow(linechart_clinical),
                                       round(nrow(linechart_clinical) * 0.03))] <- NA
linechart_grouped$lab_value[sample(nrow(linechart_grouped),
                                   round(nrow(linechart_grouped) * 0.03))] <- NA

# ═══════════════════════════════════════════════════════════
# Save all datasets in multiple formats
# ═══════════════════════════════════════════════════════════

datasets <- list(
  linechart_simple = linechart_simple,
  linechart_grouped = linechart_grouped,
  linechart_clinical = linechart_clinical,
  linechart_short = linechart_short,
  linechart_long = linechart_long,
  linechart_irregular = linechart_irregular,
  linechart_multiple = linechart_multiple,
  linechart_patterns = linechart_patterns
)

for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]

  # 1. RDA format
  assign(dataset_name, dataset)
  save(list = dataset_name, file = here("data", paste0(dataset_name, ".rda")))

  # 2. CSV format
  write.csv(dataset, file = here("data", paste0(dataset_name, ".csv")), row.names = FALSE)

  # 3. Excel format
  write_xlsx(dataset, path = here("data", paste0(dataset_name, ".xlsx")))

  # 4. Jamovi OMV format
  write_omv(dataset, here("data", paste0(dataset_name, ".omv")))

  cat("Created", dataset_name, "with", nrow(dataset), "observations\n")
}

# ═══════════════════════════════════════════════════════════
# Documentation
# ═══════════════════════════════════════════════════════════

cat("
═══════════════════════════════════════════════════════════
Test Data Generation Complete: linechart
═══════════════════════════════════════════════════════════

Total datasets: 8
Total files: 32 (8 datasets × 4 formats)

Dataset Descriptions:
─────────────────────────────────────────────────────────

1. linechart_simple (n=100)
   Purpose: Basic time series with single line
   Variables: time_point, value, temperature
   Pattern: Linear trend with noise, sinusoidal

2. linechart_grouped (n=150)
   Purpose: Grouped time series (three treatment groups)
   Variables: time_point, treatment, lab_value, patient_id
   Groups: Control, Treatment A, Treatment B
   Pattern: Different response rates by treatment

3. linechart_clinical (n=180)
   Purpose: Clinical measurements over 12 weeks
   Variables: week, disease_stage, tumor_marker, wbc_count, patient
   Groups: Early, Intermediate, Advanced
   Pattern: Disease progression patterns
   Missing: ~3% in tumor_marker

4. linechart_short (n=20)
   Purpose: Short time series (5 time points)
   Variables: month, response_group, measurement, subject
   Groups: Responder, Non-responder
   Pattern: Treatment response divergence

5. linechart_long (n=365)
   Purpose: Long daily time series (1 year)
   Variables: day, seasonal_value, cyclic_value, month
   Pattern: Seasonal and cyclic patterns

6. linechart_irregular (n=120)
   Purpose: Irregular time intervals
   Variables: time, obs_type, value, obs_id
   Groups: Type A, Type B, Type C
   Pattern: Exponential decay with irregular spacing

7. linechart_multiple (n=200)
   Purpose: Multiple measurements per time point
   Variables: visit, intervention, systolic_bp, cholesterol, patient_id
   Groups: Placebo, Drug A, Drug B, Drug C
   Pattern: Dose-response relationships

8. linechart_patterns (n=150)
   Purpose: Different trend patterns
   Variables: time_index, pattern_type, value, variance
   Groups: Increasing, Decreasing, Stable
   Pattern: Exponential, logarithmic, constant

File Formats:
─────────────────────────────────────────────────────────
✓ RDA  - Native R format (data/*.rda)
✓ CSV  - Universal format (data/*.csv)
✓ XLSX - Excel format (data/*.xlsx)
✓ OMV  - Jamovi native format (data/*.omv)

Usage Examples:
─────────────────────────────────────────────────────────

# Load data
data(linechart_simple, package = 'ClinicoPath')
data(linechart_grouped, package = 'ClinicoPath')

# Basic line chart
linechart(
  data = linechart_simple,
  xvar = 'time_point',
  yvar = 'value'
)

# Grouped with confidence intervals
linechart(
  data = linechart_grouped,
  xvar = 'time_point',
  yvar = 'lab_value',
  groupby = 'treatment',
  confidence = TRUE,
  trendline = TRUE
)

# Clinical time series
linechart(
  data = linechart_clinical,
  xvar = 'week',
  yvar = 'tumor_marker',
  groupby = 'disease_stage',
  confidence = TRUE,
  refline = 100,
  reflineLabel = 'Normal Range'
)

═══════════════════════════════════════════════════════════
")
