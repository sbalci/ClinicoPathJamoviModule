# ═══════════════════════════════════════════════════════════
# Test Data Generation: survival
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the survival jamovi function
# Function: Univariate Survival Analysis with Cox regression and Kaplan-Meier
#
# Generated: 2026-01-06
# Seed: 42
# Datasets: 8 (covering standard, dates, competing, landmark, stratified, person-time, rmst, small)

library(tibble)
library(dplyr)
library(lubridate)
library(here)

# Set seed for reproducibility
set.seed(42)

# ───────────────────────────────────────────────────────────
# Dataset 1: Standard Univariate Survival Analysis
# ───────────────────────────────────────────────────────────
# Purpose: Standard survival analysis with elapsed time
# Use case: Comparing survival between treatment groups
# N: 200 patients

n_test <- 200

survival_test <- tibble(
  # Patient ID
  PatientID = paste0("P", sprintf("%03d", 1:n_test)),

  # Elapsed time in months (follow-up time)
  elapsedtime = pmax(0.5, rnorm(n_test, mean = 36, sd = 24)),

  # Outcome: 0 = censored, 1 = event (death)
  outcome = rbinom(n_test, size = 1, prob = 0.55),

  # Treatment group (explanatory variable)
  treatment = sample(
    c("Control", "Treatment A", "Treatment B"),
    n_test,
    replace = TRUE,
    prob = c(0.35, 0.35, 0.30)
  ),

  # Age at diagnosis
  age = round(pmax(18, rnorm(n_test, mean = 65, sd = 12))),

  # Tumor stage
  stage = sample(
    c("I", "II", "III", "IV"),
    n_test,
    replace = TRUE,
    prob = c(0.15, 0.30, 0.35, 0.20)
  ),

  # Tumor grade
  grade = sample(
    c("1", "2", "3"),
    n_test,
    replace = TRUE,
    prob = c(0.25, 0.50, 0.25)
  ),

  # Sex
  sex = sample(c("Male", "Female"), n_test, replace = TRUE),

  # Performance status (ECOG)
  performance_status = sample(0:3, n_test, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),

  # Continuous biomarker
  biomarker_value = rnorm(n_test, mean = 50, sd = 15)
)

# Add realistic treatment effects
survival_test <- survival_test %>%
  mutate(
    elapsedtime = case_when(
      treatment == "Treatment A" ~ elapsedtime * 1.4,
      treatment == "Treatment B" ~ elapsedtime * 1.6,
      TRUE ~ elapsedtime
    )
  )

# Add stage-based survival differences
survival_test <- survival_test %>%
  mutate(
    elapsedtime = case_when(
      stage == "IV" ~ elapsedtime * 0.4,
      stage == "III" ~ elapsedtime * 0.6,
      stage == "II" ~ elapsedtime * 0.85,
      TRUE ~ elapsedtime
    ),
    outcome = case_when(
      stage == "IV" & elapsedtime < 24 ~ ifelse(runif(n_test) < 0.8, 1, outcome),
      stage == "I" & elapsedtime > 48 ~ ifelse(runif(n_test) < 0.3, 1, outcome),
      TRUE ~ outcome
    )
  )

# Add some missing data (~5%)
n_missing <- round(n_test * 0.05)
survival_test$biomarker_value[sample(n_test, n_missing)] <- NA

# Round elapsed time
survival_test$elapsedtime <- round(survival_test$elapsedtime, 1)

# Convert to factors
survival_test <- survival_test %>%
  mutate(
    treatment = factor(treatment, levels = c("Control", "Treatment A", "Treatment B")),
    stage = factor(stage, levels = c("I", "II", "III", "IV")),
    grade = factor(grade, levels = c("1", "2", "3")),
    sex = factor(sex),
    performance_status = factor(performance_status, levels = 0:3)
  )


# ───────────────────────────────────────────────────────────
# Dataset 2: Date-Based Survival Calculation
# ───────────────────────────────────────────────────────────
# Purpose: Calculate survival time from diagnosis and follow-up dates
# Use case: Real-world EHR data with dates
# N: 150 patients

n_dates <- 150

# Generate diagnosis dates (2018-2022)
dxdates <- sample(seq(as.Date("2018-01-01"), as.Date("2022-12-31"), by = "day"), n_dates)

survival_dates <- tibble(
  PatientID = paste0("D", sprintf("%03d", 1:n_dates)),

  # Diagnosis date (YYYY-MM-DD format)
  dxdate = as.character(dxdates),

  # Follow-up time in days
  followup_days = pmax(30, rnorm(n_dates, mean = 1095, sd = 730)),  # ~3 years

  # Event indicator
  outcome = rbinom(n_dates, size = 1, prob = 0.50),

  # Treatment
  treatment = sample(c("Surgery", "Chemotherapy", "Combined"), n_dates, replace = TRUE),

  # Clinical factors
  age = round(pmax(25, rnorm(n_dates, mean = 62, sd = 15))),
  stage = sample(c("Early", "Advanced"), n_dates, replace = TRUE, prob = c(0.6, 0.4)),
  histology = sample(
    c("Adenocarcinoma", "Squamous Cell", "Other"),
    n_dates,
    replace = TRUE,
    prob = c(0.5, 0.3, 0.2)
  )
)

# Calculate follow-up date
survival_dates <- survival_dates %>%
  mutate(
    fudate = as.character(dxdates + days(round(followup_days)))
  ) %>%
  select(-followup_days)

# Treatment effects on survival
survival_dates <- survival_dates %>%
  mutate(
    fudate_adjusted = dxdates + days(round(
      case_when(
        treatment == "Combined" ~ as.numeric(as.Date(fudate) - dxdates) * 1.5,
        treatment == "Chemotherapy" ~ as.numeric(as.Date(fudate) - dxdates) * 1.2,
        TRUE ~ as.numeric(as.Date(fudate) - dxdates)
      )
    )),
    fudate = as.character(fudate_adjusted)
  ) %>%
  select(-fudate_adjusted)

# Convert to factors
survival_dates <- survival_dates %>%
  mutate(
    treatment = factor(treatment),
    stage = factor(stage, levels = c("Early", "Advanced")),
    histology = factor(histology)
  )


# ───────────────────────────────────────────────────────────
# Dataset 3: Competing Risks Analysis
# ───────────────────────────────────────────────────────────
# Purpose: Multiple event types (competing risks)
# Use case: Death from disease vs death from other causes
# N: 180 patients

n_compete <- 180

survival_competing <- tibble(
  PatientID = paste0("C", sprintf("%03d", 1:n_compete)),

  # Elapsed time
  elapsedtime = pmax(1, rnorm(n_compete, mean = 42, sd = 28)),

  # Outcome with 4 levels for competing risks
  outcome_temp = sample(1:4, n_compete, replace = TRUE, prob = c(0.35, 0.20, 0.25, 0.20)),

  # Treatment
  treatment = sample(c("Standard", "Experimental"), n_compete, replace = TRUE),

  # Risk factors
  age = round(pmax(30, rnorm(n_compete, mean = 68, sd = 13))),
  comorbidity_index = sample(0:5, n_compete, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.05, 0.05)),
  stage = sample(c("I", "II", "III", "IV"), n_compete, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  molecular_subtype = sample(
    c("Type A", "Type B", "Type C"),
    n_compete,
    replace = TRUE,
    prob = c(0.4, 0.35, 0.25)
  )
)

# Define outcome levels for competing risks
survival_competing <- survival_competing %>%
  mutate(
    outcome = case_when(
      outcome_temp == 1 ~ "Alive w/o Disease",
      outcome_temp == 2 ~ "Alive w Disease",
      outcome_temp == 3 ~ "Dead of Disease",
      outcome_temp == 4 ~ "Dead of Other"
    )
  ) %>%
  select(-outcome_temp)

# Adjust survival times based on stage and treatment
survival_competing <- survival_competing %>%
  mutate(
    elapsedtime = case_when(
      stage == "IV" ~ elapsedtime * 0.5,
      stage == "III" ~ elapsedtime * 0.7,
      TRUE ~ elapsedtime
    ),
    elapsedtime = ifelse(treatment == "Experimental", elapsedtime * 1.3, elapsedtime)
  )

# Round elapsed time
survival_competing$elapsedtime <- round(survival_competing$elapsedtime, 1)

# Convert to factors
survival_competing <- survival_competing %>%
  mutate(
    outcome = factor(outcome, levels = c("Alive w/o Disease", "Alive w Disease", "Dead of Disease", "Dead of Other")),
    treatment = factor(treatment),
    stage = factor(stage, levels = c("I", "II", "III", "IV")),
    molecular_subtype = factor(molecular_subtype)
  )


# ───────────────────────────────────────────────────────────
# Dataset 4: Landmark Analysis
# ───────────────────────────────────────────────────────────
# Purpose: Landmark analysis to avoid guarantee-time bias
# Use case: Treatment response at 6 months predicting long-term survival
# N: 160 patients

n_landmark <- 160

survival_landmark <- tibble(
  PatientID = paste0("L", sprintf("%03d", 1:n_landmark)),

  # Elapsed time - ensure sufficient follow-up beyond landmark
  elapsedtime = pmax(3, rnorm(n_landmark, mean = 48, sd = 24)),

  # Event indicator
  outcome = rbinom(n_landmark, size = 1, prob = 0.50),

  # Response at 6 months (landmark time)
  response_6mo = sample(
    c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"),
    n_landmark,
    replace = TRUE,
    prob = c(0.25, 0.30, 0.25, 0.20)
  ),

  # Treatment
  treatment = sample(c("Therapy A", "Therapy B"), n_landmark, replace = TRUE),

  # Baseline characteristics
  age = round(pmax(20, rnorm(n_landmark, mean = 60, sd = 14))),
  baseline_tumor_size = rnorm(n_landmark, mean = 5.0, sd = 2.5),
  ki67_index = round(rnorm(n_landmark, mean = 30, sd = 15))
)

# Response at 6mo affects subsequent survival
survival_landmark <- survival_landmark %>%
  mutate(
    elapsedtime = case_when(
      response_6mo == "Complete Response" ~ elapsedtime * 1.8,
      response_6mo == "Partial Response" ~ elapsedtime * 1.4,
      response_6mo == "Stable Disease" ~ elapsedtime * 1.0,
      response_6mo == "Progressive Disease" ~ elapsedtime * 0.5
    ),
    outcome = case_when(
      response_6mo == "Progressive Disease" & elapsedtime < 12 ~
        ifelse(runif(n_landmark) < 0.7, 1, outcome),
      response_6mo == "Complete Response" & elapsedtime > 36 ~
        ifelse(runif(n_landmark) < 0.2, 1, outcome),
      TRUE ~ outcome
    )
  )

# Round values
survival_landmark$elapsedtime <- round(survival_landmark$elapsedtime, 1)
survival_landmark$baseline_tumor_size <- round(survival_landmark$baseline_tumor_size, 1)

# Convert to factors
survival_landmark <- survival_landmark %>%
  mutate(
    response_6mo = factor(
      response_6mo,
      levels = c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease")
    ),
    treatment = factor(treatment)
  )


# ───────────────────────────────────────────────────────────
# Dataset 5: Stratified Cox Regression
# ───────────────────────────────────────────────────────────
# Purpose: Non-proportional hazards requiring stratification
# Use case: Sex has different baseline hazards over time
# N: 150 patients

n_strat <- 150

survival_stratified <- tibble(
  PatientID = paste0("S", sprintf("%03d", 1:n_strat)),

  # Elapsed time
  elapsedtime = pmax(2, rnorm(n_strat, mean = 40, sd = 26)),

  # Event
  outcome = rbinom(n_strat, size = 1, prob = 0.55),

  # Stratification variable (sex - non-proportional hazards)
  sex = sample(c("Male", "Female"), n_strat, replace = TRUE),

  # Treatment (proportional hazards - can be analyzed normally)
  treatment = sample(c("Control", "Active"), n_strat, replace = TRUE),

  # Other covariates
  age = round(pmax(25, rnorm(n_strat, mean = 63, sd = 13))),
  smoking_status = sample(c("Never", "Former", "Current"), n_strat, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  bmi = round(rnorm(n_strat, mean = 27, sd = 5), 1)
)

# Create non-proportional hazards for sex
# Males have higher early hazard, females have higher late hazard
survival_stratified <- survival_stratified %>%
  mutate(
    outcome = case_when(
      sex == "Male" & elapsedtime < 24 ~
        ifelse(runif(n_strat) < 0.65, 1, outcome),  # Males worse early
      sex == "Female" & elapsedtime > 36 ~
        ifelse(runif(n_strat) < 0.60, 1, outcome),  # Females worse late
      TRUE ~ outcome
    )
  )

# Treatment effect (proportional)
survival_stratified <- survival_stratified %>%
  mutate(
    elapsedtime = ifelse(treatment == "Active", elapsedtime * 1.4, elapsedtime)
  )

# Round elapsed time
survival_stratified$elapsedtime <- round(survival_stratified$elapsedtime, 1)

# Convert to factors
survival_stratified <- survival_stratified %>%
  mutate(
    sex = factor(sex),
    treatment = factor(treatment, levels = c("Control", "Active")),
    smoking_status = factor(smoking_status, levels = c("Never", "Former", "Current"))
  )


# ───────────────────────────────────────────────────────────
# Dataset 6: Person-Time Analysis
# ───────────────────────────────────────────────────────────
# Purpose: Calculate person-time metrics and incidence rates
# Use case: Different follow-up durations, time-interval analysis
# N: 250 patients

n_pt <- 250

survival_person_time <- tibble(
  PatientID = paste0("PT", sprintf("%03d", 1:n_pt)),

  # Highly variable follow-up times
  elapsedtime = c(
    pmax(0.5, rnorm(n_pt * 0.3, mean = 12, sd = 6)),    # Short follow-up
    pmax(1, rnorm(n_pt * 0.4, mean = 36, sd = 12)),     # Medium follow-up
    pmax(2, rnorm(n_pt * 0.3, mean = 72, sd = 18))      # Long follow-up
  )[1:n_pt],

  # Event indicator
  outcome = rbinom(n_pt, size = 1, prob = 0.45),

  # Group
  cohort = sample(c("2018", "2019", "2020", "2021"), n_pt, replace = TRUE),

  # Risk factors
  age_group = sample(c("<50", "50-65", ">65"), n_pt, replace = TRUE, prob = c(0.2, 0.45, 0.35)),
  risk_category = sample(c("Low", "Intermediate", "High"), n_pt, replace = TRUE, prob = c(0.4, 0.35, 0.25)),

  # Continuous variables
  age = round(pmax(20, rnorm(n_pt, mean = 61, sd = 14))),
  creatinine = round(rnorm(n_pt, mean = 1.0, sd = 0.3), 2)
)

# Events more common in high risk and older patients
survival_person_time <- survival_person_time %>%
  mutate(
    outcome = case_when(
      risk_category == "High" ~ ifelse(runif(n_pt) < 0.70, 1, outcome),
      risk_category == "Low" ~ ifelse(runif(n_pt) < 0.25, 1, outcome),
      TRUE ~ outcome
    )
  )

# Round elapsed time
survival_person_time$elapsedtime <- round(survival_person_time$elapsedtime, 1)

# Convert to factors
survival_person_time <- survival_person_time %>%
  mutate(
    cohort = factor(cohort),
    age_group = factor(age_group, levels = c("<50", "50-65", ">65")),
    risk_category = factor(risk_category, levels = c("Low", "Intermediate", "High"))
  )


# ───────────────────────────────────────────────────────────
# Dataset 7: RMST Analysis
# ───────────────────────────────────────────────────────────
# Purpose: Restricted Mean Survival Time analysis
# Use case: When median survival cannot be estimated, or for specific time periods
# N: 180 patients

n_rmst <- 180

survival_rmst <- tibble(
  PatientID = paste0("R", sprintf("%03d", 1:n_rmst)),

  # Elapsed time with good long-term survivors
  elapsedtime = pmax(1, rnorm(n_rmst, mean = 45, sd = 30)),

  # Event indicator (lower event rate for RMST scenario)
  outcome = rbinom(n_rmst, size = 1, prob = 0.40),

  # Treatment
  treatment = sample(c("Standard Care", "Novel Therapy"), n_rmst, replace = TRUE),

  # Prognostic factors
  age = round(pmax(30, rnorm(n_rmst, mean = 59, sd = 12))),
  tumor_burden = sample(c("Low", "Moderate", "High"), n_rmst, replace = TRUE, prob = c(0.3, 0.45, 0.25)),
  performance_score = sample(0:2, n_rmst, replace = TRUE, prob = c(0.5, 0.35, 0.15)),
  prior_therapy = sample(c("None", "One", "Two or More"), n_rmst, replace = TRUE, prob = c(0.4, 0.35, 0.25))
)

# Novel therapy improves survival
survival_rmst <- survival_rmst %>%
  mutate(
    elapsedtime = case_when(
      treatment == "Novel Therapy" ~ elapsedtime * 1.5,
      TRUE ~ elapsedtime
    ),
    outcome = case_when(
      treatment == "Novel Therapy" & elapsedtime > 48 ~
        ifelse(runif(n_rmst) < 0.20, 1, outcome),
      treatment == "Standard Care" & elapsedtime > 48 ~
        ifelse(runif(n_rmst) < 0.50, 1, outcome),
      TRUE ~ outcome
    )
  )

# Tumor burden affects survival
survival_rmst <- survival_rmst %>%
  mutate(
    elapsedtime = case_when(
      tumor_burden == "High" ~ elapsedtime * 0.6,
      tumor_burden == "Low" ~ elapsedtime * 1.3,
      TRUE ~ elapsedtime
    )
  )

# Round elapsed time
survival_rmst$elapsedtime <- round(survival_rmst$elapsedtime, 1)

# Convert to factors
survival_rmst <- survival_rmst %>%
  mutate(
    treatment = factor(treatment),
    tumor_burden = factor(tumor_burden, levels = c("Low", "Moderate", "High")),
    performance_score = factor(performance_score, levels = 0:2),
    prior_therapy = factor(prior_therapy, levels = c("None", "One", "Two or More"))
  )


# ───────────────────────────────────────────────────────────
# Dataset 8: Small Dataset for Quick Testing
# ───────────────────────────────────────────────────────────
# Purpose: Minimal dataset for rapid testing
# N: 30 patients

n_small <- 30

survival_small <- tibble(
  PatientID = paste0("Q", sprintf("%02d", 1:n_small)),
  elapsedtime = round(pmax(1, rnorm(n_small, mean = 24, sd = 12)), 1),
  outcome = rbinom(n_small, size = 1, prob = 0.50),
  treatment = factor(sample(c("A", "B"), n_small, replace = TRUE)),
  age = round(pmax(20, rnorm(n_small, mean = 60, sd = 15))),
  stage = factor(sample(c("Early", "Late"), n_small, replace = TRUE))
)


# ═══════════════════════════════════════════════════════════
# Save All Datasets in Multiple Formats
# ═══════════════════════════════════════════════════════════

datasets <- list(
  survival_test = survival_test,
  survival_dates = survival_dates,
  survival_competing = survival_competing,
  survival_landmark = survival_landmark,
  survival_stratified = survival_stratified,
  survival_person_time = survival_person_time,
  survival_rmst = survival_rmst,
  survival_small = survival_small
)

# Save each dataset in all 4 formats
for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]

  # 1. RDA format (native R)
  assign(dataset_name, dataset)
  save(list = dataset_name, file = here::here("data", paste0(dataset_name, ".rda")))

  # 2. CSV format
  write.csv(dataset, file = here::here("data", paste0(dataset_name, ".csv")), row.names = FALSE)

  # 3. Excel format
  writexl::write_xlsx(dataset, path = here::here("data", paste0(dataset_name, ".xlsx")))

  # 4. Jamovi format (OMV)
  jmvReadWrite::write_omv(dataset, here::here("data", paste0(dataset_name, ".omv")))

  cat("✓ Generated", dataset_name, "in all formats\n")
}

# ═══════════════════════════════════════════════════════════
# Dataset Documentation Summary
# ═══════════════════════════════════════════════════════════

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("survival Test Data Generation Complete\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Generated Datasets:\n\n")

cat("1. survival_test (n=200)\n")
cat("   Purpose: Standard univariate survival analysis\n")
cat("   Variables:", ncol(survival_test), "\n")
cat("   Use: Compare survival between treatment groups\n\n")

cat("2. survival_dates (n=150)\n")
cat("   Purpose: Date-based survival time calculation\n")
cat("   Variables:", ncol(survival_dates), "\n")
cat("   Use: Calculate time from diagnosis/follow-up dates\n\n")

cat("3. survival_competing (n=180)\n")
cat("   Purpose: Competing risks analysis\n")
cat("   Variables:", ncol(survival_competing), "\n")
cat("   Use: Multiple event types (death of disease, death other causes)\n\n")

cat("4. survival_landmark (n=160)\n")
cat("   Purpose: Landmark analysis\n")
cat("   Variables:", ncol(survival_landmark), "\n")
cat("   Use: Conditional survival from landmark time point\n\n")

cat("5. survival_stratified (n=150)\n")
cat("   Purpose: Stratified Cox regression\n")
cat("   Variables:", ncol(survival_stratified), "\n")
cat("   Use: Handle non-proportional hazards via stratification\n\n")

cat("6. survival_person_time (n=250)\n")
cat("   Purpose: Person-time analysis\n")
cat("   Variables:", ncol(survival_person_time), "\n")
cat("   Use: Incidence rates, time-interval analysis\n\n")

cat("7. survival_rmst (n=180)\n")
cat("   Purpose: Restricted Mean Survival Time\n")
cat("   Variables:", ncol(survival_rmst), "\n")
cat("   Use: RMST when median survival cannot be estimated\n\n")

cat("8. survival_small (n=30)\n")
cat("   Purpose: Quick testing\n")
cat("   Variables:", ncol(survival_small), "\n")
cat("   Use: Rapid functionality checks\n\n")

cat("Total Files Generated: 32 (8 datasets × 4 formats)\n")
cat("Formats: RDA, CSV, XLSX, OMV\n")
cat("Seed: 42 (for reproducibility)\n\n")

cat("Usage Example:\n")
cat("  library(ClinicoPath)\n")
cat("  data(survival_test)\n")
cat("  survival(\n")
cat("    data = survival_test,\n")
cat("    elapsedtime = 'elapsedtime',\n")
cat("    outcome = 'outcome',\n")
cat("    explanatory = 'treatment'\n")
cat("  )\n\n")

cat("Files Location:\n")
cat("  Data: data/survival_*.{rda,csv,xlsx,omv}\n")
cat("  Tests: tests/testthat/test-survival-*.R\n")
cat("  Examples: inst/examples/survival_example.R\n")
cat("  Documentation: docs/test-data/SURVIVAL_TEST_DATA_SUMMARY.md\n\n")

cat("═══════════════════════════════════════════════════════════\n")
