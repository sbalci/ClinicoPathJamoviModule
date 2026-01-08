# ═══════════════════════════════════════════════════════════
# Test Data Generation: timeinterval
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the timeinterval jamovi function
# which calculates time intervals for survival analysis and person-time calculations
#
# Generated: 2026-01-06
# Seed: 42
# Function: Comprehensive Time Interval Calculator

library(tibble)
library(dplyr)
library(lubridate)
library(here)
set.seed(42)

# ═══════════════════════════════════════════════════════════
# 1. BASIC TIME INTERVAL DATA (YYYY-MM-DD FORMAT)
# ═══════════════════════════════════════════════════════════

n_patients <- 100

# Generate base dates
base_date <- as.Date("2020-01-01")

timeinterval_test <- tibble(
  # Patient identifiers
  patient_id = paste0("PT", sprintf("%03d", 1:n_patients)),

  # Diagnosis/start dates (2020-2021)
  diagnosis_date = base_date + sample(0:365, n_patients, replace = TRUE),

  # Follow-up/end dates (varying durations)
  followup_date = diagnosis_date + sample(30:1095, n_patients, replace = TRUE),

  # Clinical covariates
  age = round(rnorm(n_patients, mean = 65, sd = 12)),
  sex = sample(c("Male", "Female"), n_patients, replace = TRUE),
  treatment = sample(c("Surgery", "Chemotherapy", "Radiation", "Combined"),
                     n_patients, replace = TRUE),
  stage = sample(c("I", "II", "III", "IV"), n_patients, replace = TRUE,
                 prob = c(0.2, 0.3, 0.3, 0.2)),

  # Event indicator
  event_occurred = sample(0:1, n_patients, replace = TRUE, prob = c(0.4, 0.6))
)

# Convert dates to character (YYYY-MM-DD format)
timeinterval_test <- timeinterval_test %>%
  mutate(
    diagnosis_date = as.character(diagnosis_date),
    followup_date = as.character(followup_date)
  )

# ═══════════════════════════════════════════════════════════
# 2. MULTIPLE DATE FORMATS
# ═══════════════════════════════════════════════════════════

n_dates <- 50

# YMD format (YYYY-MM-DD)
timeinterval_ymd <- tibble(
  id = paste0("ID", 1:n_dates),
  start_date = as.character(seq.Date(as.Date("2021-01-01"),
                                     by = "weeks", length.out = n_dates)),
  end_date = as.character(seq.Date(as.Date("2021-01-01"),
                                   by = "weeks", length.out = n_dates) +
                          sample(90:365, n_dates, replace = TRUE))
)

# DMY format (DD-MM-YYYY)
timeinterval_dmy <- tibble(
  id = paste0("ID", 1:n_dates),
  start_date = format(seq.Date(as.Date("2021-01-01"),
                               by = "weeks", length.out = n_dates), "%d-%m-%Y"),
  end_date = format(seq.Date(as.Date("2021-01-01"),
                             by = "weeks", length.out = n_dates) +
                    sample(90:365, n_dates, replace = TRUE), "%d-%m-%Y")
)

# MDY format (MM-DD-YYYY)
timeinterval_mdy <- tibble(
  id = paste0("ID", 1:n_dates),
  start_date = format(seq.Date(as.Date("2021-01-01"),
                               by = "weeks", length.out = n_dates), "%m-%d-%Y"),
  end_date = format(seq.Date(as.Date("2021-01-01"),
                             by = "weeks", length.out = n_dates) +
                    sample(90:365, n_dates, replace = TRUE), "%m-%d-%Y")
)

# YMDHMS format (with time)
timeinterval_ymdhms <- tibble(
  id = paste0("ID", 1:n_dates),
  start_datetime = as.character(seq.POSIXt(
    as.POSIXct("2021-01-01 08:00:00"),
    by = "days", length.out = n_dates
  )),
  end_datetime = as.character(seq.POSIXt(
    as.POSIXct("2021-01-01 08:00:00"),
    by = "days", length.out = n_dates
  ) + sample(3600:259200, n_dates, replace = TRUE))  # 1 hour to 3 days
)

# ═══════════════════════════════════════════════════════════
# 3. LANDMARK ANALYSIS DATA
# ═══════════════════════════════════════════════════════════

n_landmark <- 80

timeinterval_landmark <- tibble(
  patient_id = paste0("PT", sprintf("%03d", 1:n_landmark)),
  enrollment_date = as.character(as.Date("2019-01-01") +
                                sample(0:365, n_landmark, replace = TRUE)),
  last_contact = as.character(as.Date("2019-01-01") +
                             sample(0:365, n_landmark, replace = TRUE) +
                             sample(180:1095, n_landmark, replace = TRUE)),
  treatment_arm = sample(c("Experimental", "Standard", "Control"),
                        n_landmark, replace = TRUE),
  biomarker_positive = sample(c("Yes", "No"), n_landmark, replace = TRUE)
)

# ═══════════════════════════════════════════════════════════
# 4. DATA QUALITY ISSUES
# ═══════════════════════════════════════════════════════════

n_quality <- 60

timeinterval_quality <- tibble(
  id = paste0("ID", 1:n_quality),
  start_date = as.character(as.Date("2020-06-01") + sample(0:180, n_quality, replace = TRUE)),
  end_date = as.character(as.Date("2020-06-01") + sample(0:180, n_quality, replace = TRUE) +
                         sample(-30:365, n_quality, replace = TRUE))  # Some negative intervals
)

# Add missing values (~10%)
n_missing <- round(n_quality * 0.1)
timeinterval_quality$start_date[sample(n_quality, n_missing)] <- NA
timeinterval_quality$end_date[sample(n_quality, n_missing)] <- NA

# ═══════════════════════════════════════════════════════════
# 5. EXTREME VALUES
# ═══════════════════════════════════════════════════════════

n_extreme <- 40

timeinterval_extreme <- tibble(
  id = paste0("ID", 1:n_extreme),
  start_date = as.character(as.Date("2015-01-01")),
  end_date = c(
    # Normal follow-up (1-3 years)
    as.character(as.Date("2015-01-01") + sample(365:1095, 20, replace = TRUE)),
    # Very short follow-up (<1 month)
    as.character(as.Date("2015-01-01") + sample(1:30, 10, replace = TRUE)),
    # Very long follow-up (>10 years)
    as.character(as.Date("2015-01-01") + sample(3650:5475, 10, replace = TRUE))
  )
)

# ═══════════════════════════════════════════════════════════
# 6. SMALL AND LARGE DATASETS
# ═══════════════════════════════════════════════════════════

# Minimal dataset
timeinterval_small <- tibble(
  id = paste0("ID", 1:5),
  start = as.character(as.Date("2022-01-01") + 0:4),
  end = as.character(as.Date("2022-01-01") + 0:4 + sample(30:180, 5, replace = TRUE))
)

# Large dataset
n_large <- 500
timeinterval_large <- tibble(
  id = paste0("ID", sprintf("%04d", 1:n_large)),
  dx_date = as.character(as.Date("2018-01-01") + sample(0:730, n_large, replace = TRUE)),
  fu_date = as.character(as.Date("2018-01-01") + sample(0:730, n_large, replace = TRUE) +
                        sample(30:1460, n_large, replace = TRUE)),
  cohort = sample(paste0("Cohort_", LETTERS[1:5]), n_large, replace = TRUE)
)

# ═══════════════════════════════════════════════════════════
# 7. CLINICAL TRIAL SIMULATION
# ═══════════════════════════════════════════════════════════

n_trial <- 150

timeinterval_trial <- tibble(
  patient_id = paste0("TRIAL-", sprintf("%03d", 1:n_trial)),

  # Study dates
  enrollment_date = as.character(seq.Date(as.Date("2020-01-01"),
                                         by = "days",
                                         length.out = n_trial)),

  # Follow-up dates (realistic trial duration: 6-24 months)
  followup_date = as.character(seq.Date(as.Date("2020-01-01"),
                                       by = "days",
                                       length.out = n_trial) +
                              sample(180:730, n_trial, replace = TRUE)),

  # Trial characteristics
  treatment_arm = rep(c("Arm A", "Arm B", "Arm C"), length.out = n_trial),
  site = sample(paste0("Site_", 1:10), n_trial, replace = TRUE),
  age = round(rnorm(n_trial, 62, 10)),
  ecog_ps = sample(0:2, n_trial, replace = TRUE, prob = c(0.4, 0.4, 0.2)),

  # Outcomes
  progression = sample(0:1, n_trial, replace = TRUE, prob = c(0.5, 0.5)),
  death = sample(0:1, n_trial, replace = TRUE, prob = c(0.7, 0.3))
)

# ═══════════════════════════════════════════════════════════
# 8. DIFFERENT TIME UNITS SCENARIOS
# ═══════════════════════════════════════════════════════════

# Short-term study (days/weeks)
timeinterval_shortterm <- tibble(
  id = paste0("ID", 1:30),
  admission_date = as.character(as.Date("2023-01-01") + sample(0:60, 30, replace = TRUE)),
  discharge_date = as.character(as.Date("2023-01-01") + sample(0:60, 30, replace = TRUE) +
                               sample(1:14, 30, replace = TRUE))  # 1-14 days
)

# Long-term study (years)
timeinterval_longterm <- tibble(
  id = paste0("ID", 1:40),
  baseline_date = as.character(as.Date("2010-01-01")),
  last_followup = as.character(as.Date("2010-01-01") +
                              sample(1825:5475, 40, replace = TRUE))  # 5-15 years
)

# ═══════════════════════════════════════════════════════════
# 9. SPECIAL EDGE CASES
# ═══════════════════════════════════════════════════════════

# Same-day intervals
timeinterval_sameday <- tibble(
  id = paste0("ID", 1:10),
  start = rep(as.character(as.Date("2022-06-15")), 10),
  end = rep(as.character(as.Date("2022-06-15")), 10)
)

# Negative intervals (end before start)
timeinterval_negative <- tibble(
  id = paste0("ID", 1:15),
  start_date = as.character(as.Date("2021-01-01") + sample(100:200, 15, replace = TRUE)),
  end_date = as.character(as.Date("2021-01-01") + sample(0:99, 15, replace = TRUE))
)

# All missing dates
timeinterval_allmissing <- tibble(
  id = paste0("ID", 1:10),
  start_date = rep(NA_character_, 10),
  end_date = rep(NA_character_, 10)
)

# Mixed format (should fail or warn)
timeinterval_mixedformat <- tibble(
  id = paste0("ID", 1:20),
  start_date = c(
    rep("2021-01-15", 10),  # YYYY-MM-DD
    rep("15-01-2021", 10)   # DD-MM-YYYY
  ),
  end_date = rep(as.character(as.Date("2021-06-15")), 20)
)

# ═══════════════════════════════════════════════════════════
# SAVE ALL DATASETS
# ═══════════════════════════════════════════════════════════

datasets <- list(
  timeinterval_test = timeinterval_test,
  timeinterval_ymd = timeinterval_ymd,
  timeinterval_dmy = timeinterval_dmy,
  timeinterval_mdy = timeinterval_mdy,
  timeinterval_ymdhms = timeinterval_ymdhms,
  timeinterval_landmark = timeinterval_landmark,
  timeinterval_quality = timeinterval_quality,
  timeinterval_extreme = timeinterval_extreme,
  timeinterval_small = timeinterval_small,
  timeinterval_large = timeinterval_large,
  timeinterval_trial = timeinterval_trial,
  timeinterval_shortterm = timeinterval_shortterm,
  timeinterval_longterm = timeinterval_longterm,
  timeinterval_sameday = timeinterval_sameday,
  timeinterval_negative = timeinterval_negative,
  timeinterval_allmissing = timeinterval_allmissing,
  timeinterval_mixedformat = timeinterval_mixedformat
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
                          fleOut = here::here("data", paste0(dataset_name, ".omv")),
                          frcWrt = TRUE)
}

# ═══════════════════════════════════════════════════════════
# DOCUMENTATION
# ═══════════════════════════════════════════════════════════

cat("
╔════════════════════════════════════════════════════════════╗
║     Time Interval Test Data Generation Summary            ║
╚════════════════════════════════════════════════════════════╝

DATASETS CREATED:
─────────────────────────────────────────────────────────────

1. timeinterval_test (Main test dataset)
   • Observations: 100 patients
   • Variables: 8 (patient_id, diagnosis_date, followup_date, age, sex, treatment, stage, event_occurred)
   • Date format: YYYY-MM-DD (character)
   • Use case: Basic time interval calculation
   • Duration range: 1 month to 3 years

2. timeinterval_ymd (YMD format)
   • Observations: 50
   • Variables: 3 (id, start_date, end_date)
   • Format: YYYY-MM-DD
   • Use case: Standard date format testing

3. timeinterval_dmy (DMY format)
   • Observations: 50
   • Variables: 3
   • Format: DD-MM-YYYY
   • Use case: European date format

4. timeinterval_mdy (MDY format)
   • Observations: 50
   • Variables: 3
   • Format: MM-DD-YYYY
   • Use case: US date format

5. timeinterval_ymdhms (DateTime format)
   • Observations: 50
   • Variables: 3
   • Format: YYYY-MM-DD HH:MM:SS
   • Use case: Datetime with hours/minutes/seconds

6. timeinterval_landmark (Landmark analysis)
   • Observations: 80 patients
   • Variables: 5
   • Use case: Landmark analysis testing (conditional survival)

7. timeinterval_quality (Data quality issues)
   • Observations: 60
   • Variables: 3
   • Issues: Negative intervals, missing values (~10%)
   • Use case: Quality assessment testing

8. timeinterval_extreme (Extreme values)
   • Observations: 40
   • Variables: 3
   • Includes: Very short (<1 month), normal, very long (>10 years)
   • Use case: Outlier detection

9. timeinterval_small (Minimal dataset)
   • Observations: 5
   • Use case: Edge case testing

10. timeinterval_large (Performance testing)
    • Observations: 500
    • Use case: Large dataset handling

11. timeinterval_trial (Clinical trial)
    • Observations: 150 patients
    • Variables: 9
    • Use case: Realistic trial simulation

12. timeinterval_shortterm (Short duration)
    • Observations: 30
    • Duration: 1-14 days
    • Use case: Days/weeks output unit testing

13. timeinterval_longterm (Long duration)
    • Observations: 40
    • Duration: 5-15 years
    • Use case: Years output unit testing

14. timeinterval_sameday (Zero intervals)
    • Observations: 10
    • Duration: 0 days (same start and end)
    • Use case: Zero-interval handling

15. timeinterval_negative (Negative intervals)
    • Observations: 15
    • Issue: End date before start date
    • Use case: Negative interval detection

16. timeinterval_allmissing (All NA)
    • Observations: 10
    • Issue: All dates are NA
    • Use case: Complete missing data handling

17. timeinterval_mixedformat (Mixed formats)
    • Observations: 20
    • Issue: Inconsistent date formats within column
    • Use case: Format validation

─────────────────────────────────────────────────────────────
FILE FORMATS:
─────────────────────────────────────────────────────────────
Each dataset saved in 4 formats:
  ✓ .rda   - R data format (fast loading)
  ✓ .csv   - CSV format (universal)
  ✓ .xlsx  - Excel format (clinician-friendly)
  ✓ .omv   - Jamovi format (native)

─────────────────────────────────────────────────────────────
DATE FORMATS SUPPORTED:
─────────────────────────────────────────────────────────────

auto        : Automatic detection
ymd         : YYYY-MM-DD (ISO 8601)
dmy         : DD-MM-YYYY (European)
mdy         : MM-DD-YYYY (US)
ydm         : YYYY-DD-MM
myd         : MM-YYYY-DD
dym         : DD-YYYY-MM
ymdhms      : YYYY-MM-DD HH:MM:SS

─────────────────────────────────────────────────────────────
TIME UNITS:
─────────────────────────────────────────────────────────────

days        : Day-level precision
weeks       : 7-day periods
months      : 30.44-day months (standardized) or calendar months
years       : 365.25-day years (standardized) or calendar years

─────────────────────────────────────────────────────────────
USAGE EXAMPLES:
─────────────────────────────────────────────────────────────

# Load test data
data(timeinterval_test, package = 'ClinicoPath')

# Basic time interval calculation
timeinterval(
  data = timeinterval_test,
  dx_date = 'diagnosis_date',
  fu_date = 'followup_date',
  time_format = 'ymd',
  output_unit = 'months'
)

# With landmark analysis
timeinterval(
  data = timeinterval_landmark,
  dx_date = 'enrollment_date',
  fu_date = 'last_contact',
  use_landmark = TRUE,
  landmark_time = 6,
  output_unit = 'months'
)

# With quality assessment
timeinterval(
  data = timeinterval_quality,
  dx_date = 'start_date',
  fu_date = 'end_date',
  remove_negative = TRUE,
  remove_extreme = TRUE,
  include_quality_metrics = TRUE
)

# Clinical trial analysis
timeinterval(
  data = timeinterval_trial,
  dx_date = 'enrollment_date',
  fu_date = 'followup_date',
  output_unit = 'months',
  add_times = TRUE,
  confidence_level = 95
)

─────────────────────────────────────────────────────────────
Generated: 2026-01-06
Seed: 42
Package: ClinicoPath
─────────────────────────────────────────────────────────────
")
