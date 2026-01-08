# ═══════════════════════════════════════════════════════════
# Test Data Generation: multisurvival
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the multisurvival jamovi function
# in the ClinicoPath module. Multivariable survival analysis using Cox proportional
# hazards regression.
#
# Generated: 2026-01-06
# Seed: 42
# Datasets: 8 comprehensive scenarios

library(tibble)
library(dplyr)
library(writexl)
library(here)
set.seed(42)

# ───────────────────────────────────────────────────────────
# Dataset 1: Main Test Dataset (multisurvival_test)
# ───────────────────────────────────────────────────────────
# Standard multivariable Cox regression with numeric elapsed time

n_main <- 200

multisurvival_test <- tibble(
  # Patient ID
  PatientID = paste0("PT", sprintf("%03d", 1:n_main)),
  
  # Follow-up time (months, 1-120)
  elapsedtime = pmax(1, rnorm(n_main, mean = 36, sd = 24)),
  
  # Event status (0 = censored, 1 = event)
  outcome = sample(c(0, 1), n_main, replace = TRUE, prob = c(0.4, 0.6)),
  
  # Treatment arm
  treatment = sample(c("Control", "Treatment A", "Treatment B"), 
                    n_main, replace = TRUE),
  
  # Tumor stage (I-IV)
  stage = sample(c("I", "II", "III", "IV"), n_main, replace = TRUE,
                prob = c(0.2, 0.3, 0.3, 0.2)),
  
  # Tumor grade (Well, Moderate, Poor)
  grade = sample(c("Well", "Moderate", "Poor"), n_main, replace = TRUE,
                prob = c(0.3, 0.5, 0.2)),
  
  # Gender
  sex = sample(c("Male", "Female"), n_main, replace = TRUE),
  
  # Age (years)
  age = round(rnorm(n_main, mean = 65, sd = 12)),
  
  # Number of positive nodes
  nodes = pmax(0, round(rnorm(n_main, mean = 3, sd = 4))),
  
  # Biomarker continuous (Ki67 %)
  biomarker = pmax(0, pmin(100, rnorm(n_main, mean = 30, sd = 20))),
  
  # Performance status (0-2)
  performance_status = sample(0:2, n_main, replace = TRUE, 
                             prob = c(0.5, 0.3, 0.2))
)

# Add realistic correlations
# Higher stage → shorter survival, higher event rate
multisurvival_test <- multisurvival_test %>%
  mutate(
    elapsedtime = case_when(
      stage == "IV" ~ elapsedtime * 0.4,
      stage == "III" ~ elapsedtime * 0.6,
      stage == "II" ~ elapsedtime * 0.8,
      TRUE ~ elapsedtime
    ),
    outcome = case_when(
      stage == "IV" & elapsedtime < 12 ~ 1,
      stage == "III" & elapsedtime < 24 ~ ifelse(runif(n_main) < 0.8, 1, outcome),
      TRUE ~ outcome
    )
  )

# Higher nodes → worse outcomes
multisurvival_test <- multisurvival_test %>%
  mutate(
    elapsedtime = ifelse(nodes > 5, elapsedtime * 0.7, elapsedtime),
    outcome = ifelse(nodes > 10, 1, outcome)
  )

# Treatment effect
multisurvival_test <- multisurvival_test %>%
  mutate(
    elapsedtime = case_when(
      treatment == "Treatment A" ~ elapsedtime * 1.3,
      treatment == "Treatment B" ~ elapsedtime * 1.5,
      TRUE ~ elapsedtime
    )
  )

# Add 5% missing data in biomarker
n_missing <- round(n_main * 0.05)
multisurvival_test$biomarker[sample(n_main, n_missing)] <- NA

# Save Dataset 1
save(multisurvival_test, file = here("data", "multisurvival_test.rda"))
write.csv(multisurvival_test, file = here("data", "multisurvival_test.csv"), row.names = FALSE)
write_xlsx(multisurvival_test, path = here("data", "multisurvival_test.xlsx"))
jmvReadWrite::write_omv(multisurvival_test, here("data", "multisurvival_test.omv"))

# ───────────────────────────────────────────────────────────
# Dataset 2: Date-Based Dataset (multisurvival_dates)
# ───────────────────────────────────────────────────────────
# Using diagnosis and follow-up dates to calculate survival time

n_dates <- 150

# Base date for generating dates
base_date <- as.Date("2018-01-01")

# Generate diagnosis dates (2018-2020)
diagnosis_dates <- base_date + sample(0:730, n_dates, replace = TRUE)

# Generate follow-up dates (6 months to 5 years after diagnosis)
followup_days <- round(runif(n_dates, min = 180, max = 1825))
followup_dates <- diagnosis_dates + followup_days

multisurvival_dates <- tibble(
  PatientID = paste0("DT", sprintf("%03d", 1:n_dates)),
  
  # Dates in YYYY-MM-DD format
  dxdate = as.character(diagnosis_dates),
  fudate = as.character(followup_dates),
  
  # Outcome
  outcome = sample(c("Alive", "Dead"), n_dates, replace = TRUE, prob = c(0.5, 0.5)),
  
  # Clinical variables
  treatment = sample(c("Surgery", "Chemotherapy", "Both"), n_dates, replace = TRUE),
  stage = sample(c("I", "II", "III", "IV"), n_dates, replace = TRUE,
                prob = c(0.25, 0.30, 0.25, 0.20)),
  grade = sample(c("G1", "G2", "G3"), n_dates, replace = TRUE,
                prob = c(0.3, 0.5, 0.2)),
  age = round(rnorm(n_dates, 62, 13)),
  nodes = pmax(0, round(rnorm(n_dates, 2, 3)))
)

# Save Dataset 2
save(multisurvival_dates, file = here("data", "multisurvival_dates.rda"))
write.csv(multisurvival_dates, file = here("data", "multisurvival_dates.csv"), row.names = FALSE)
write_xlsx(multisurvival_dates, path = here("data", "multisurvival_dates.xlsx"))
jmvReadWrite::write_omv(multisurvival_dates, here("data", "multisurvival_dates.omv"))

# ───────────────────────────────────────────────────────────
# Dataset 3: Competing Risks Dataset (multisurvival_competing)
# ───────────────────────────────────────────────────────────
# Multiple event types: dead of disease, dead of other, alive with/without disease

n_compete <- 180

multisurvival_competing <- tibble(
  PatientID = paste0("CR", sprintf("%03d", 1:n_compete)),
  
  elapsedtime = pmax(1, rnorm(n_compete, mean = 40, sd = 25)),
  
  # Outcome with multiple levels for competing risks
  outcome = sample(
    c("Alive w/o Disease", "Alive w Disease", "Dead of Disease", "Dead of Other"),
    n_compete, replace = TRUE,
    prob = c(0.30, 0.20, 0.35, 0.15)
  ),
  
  # Explanatory variables
  treatment = sample(c("Standard", "Experimental"), n_compete, replace = TRUE),
  stage = sample(c("Early", "Advanced"), n_compete, replace = TRUE),
  age_group = sample(c("<60", "60-70", ">70"), n_compete, replace = TRUE,
                    prob = c(0.3, 0.4, 0.3)),
  comorbidity = sample(c("None", "Mild", "Severe"), n_compete, replace = TRUE,
                      prob = c(0.4, 0.4, 0.2)),
  
  # Continuous variables
  age = round(rnorm(n_compete, 67, 11)),
  charlson_score = pmax(0, round(rnorm(n_compete, 3, 2)))
)

# Add realistic correlations
# Advanced stage → more likely dead of disease
multisurvival_competing <- multisurvival_competing %>%
  mutate(
    outcome = case_when(
      stage == "Advanced" & elapsedtime < 24 ~ 
        sample(c("Dead of Disease", "Alive w Disease"), n_compete, replace = TRUE, prob = c(0.7, 0.3)),
      TRUE ~ outcome
    )
  )

# Higher comorbidity → more likely dead of other causes
multisurvival_competing <- multisurvival_competing %>%
  mutate(
    outcome = case_when(
      comorbidity == "Severe" & runif(n_compete) < 0.3 ~ "Dead of Other",
      TRUE ~ outcome
    )
  )

# Save Dataset 3
save(multisurvival_competing, file = here("data", "multisurvival_competing.rda"))
write.csv(multisurvival_competing, file = here("data", "multisurvival_competing.csv"), row.names = FALSE)
write_xlsx(multisurvival_competing, path = here("data", "multisurvival_competing.xlsx"))
jmvReadWrite::write_omv(multisurvival_competing, here("data", "multisurvival_competing.omv"))

# ───────────────────────────────────────────────────────────
# Dataset 4: Risk Stratification Dataset (multisurvival_risk)
# ───────────────────────────────────────────────────────────
# For testing risk score calculation and grouping

n_risk <- 200

multisurvival_risk <- tibble(
  PatientID = paste0("RS", sprintf("%03d", 1:n_risk)),
  
  elapsedtime = pmax(1, rnorm(n_risk, mean = 48, sd = 30)),
  outcome = sample(c(0, 1), n_risk, replace = TRUE, prob = c(0.35, 0.65)),
  
  # Multiple prognostic factors for risk score
  stage = sample(c("I", "II", "III", "IV"), n_risk, replace = TRUE,
                prob = c(0.15, 0.30, 0.35, 0.20)),
  grade = sample(c("Low", "Intermediate", "High"), n_risk, replace = TRUE,
                prob = c(0.25, 0.50, 0.25)),
  molecular_subtype = sample(c("Luminal A", "Luminal B", "HER2+", "Triple Negative"),
                            n_risk, replace = TRUE, 
                            prob = c(0.4, 0.3, 0.15, 0.15)),
  
  # Continuous prognostic factors
  age = round(rnorm(n_risk, 58, 14)),
  tumor_size = pmax(0.5, rnorm(n_risk, 3.2, 1.8)),  # cm
  ki67 = pmax(0, pmin(100, rnorm(n_risk, 25, 15))),  # %
  nodes_positive = pmax(0, round(rpois(n_risk, lambda = 2)))
)

# Add strong prognostic correlations for clear risk groups
multisurvival_risk <- multisurvival_risk %>%
  mutate(
    # High-risk features → shorter survival
    elapsedtime = case_when(
      stage == "IV" & molecular_subtype == "Triple Negative" ~ elapsedtime * 0.3,
      stage == "III" & grade == "High" ~ elapsedtime * 0.5,
      stage == "II" & grade == "Intermediate" ~ elapsedtime * 0.7,
      stage == "I" & grade == "Low" ~ elapsedtime * 1.3,
      TRUE ~ elapsedtime
    ),
    # High-risk → higher event rate
    outcome = case_when(
      stage == "IV" ~ 1,
      stage == "III" & grade == "High" ~ ifelse(runif(n_risk) < 0.8, 1, outcome),
      molecular_subtype == "Triple Negative" ~ ifelse(runif(n_risk) < 0.7, 1, outcome),
      TRUE ~ outcome
    )
  )

# Save Dataset 4
save(multisurvival_risk, file = here("data", "multisurvival_risk.rda"))
write.csv(multisurvival_risk, file = here("data", "multisurvival_risk.csv"), row.names = FALSE)
write_xlsx(multisurvival_risk, path = here("data", "multisurvival_risk.xlsx"))
jmvReadWrite::write_omv(multisurvival_risk, here("data", "multisurvival_risk.omv"))

# ───────────────────────────────────────────────────────────
# Dataset 5: Landmark Analysis Dataset (multisurvival_landmark)
# ───────────────────────────────────────────────────────────
# For testing landmark time analysis (e.g., 3-month landmark)

n_landmark <- 160

multisurvival_landmark <- tibble(
  PatientID = paste0("LM", sprintf("%03d", 1:n_landmark)),
  
  # Survival time (some < 3 months, some > 3 months)
  elapsedtime = pmax(0.5, c(
    rnorm(40, mean = 1.5, sd = 0.8),   # Early failures
    rnorm(120, mean = 30, sd = 20)     # Long-term survivors
  )),
  
  outcome = c(
    rep(1, 30),  # Most early failures have events
    sample(c(0, 1), 130, replace = TRUE, prob = c(0.5, 0.5))
  ),
  
  # Treatment response at 3 months (for landmark analysis)
  response_3mo = sample(c("Complete Response", "Partial Response", 
                         "Stable Disease", "Progression"),
                       n_landmark, replace = TRUE,
                       prob = c(0.2, 0.3, 0.3, 0.2)),
  
  # Baseline variables
  treatment = sample(c("Immunotherapy", "Chemotherapy"), 
                    n_landmark, replace = TRUE),
  pdl1_status = sample(c("Positive", "Negative"), 
                      n_landmark, replace = TRUE, prob = c(0.4, 0.6)),
  age = round(rnorm(n_landmark, 64, 12))
)

# Patients with progression at 3 months → worse outcomes
multisurvival_landmark <- multisurvival_landmark %>%
  mutate(
    elapsedtime = case_when(
      response_3mo == "Complete Response" ~ elapsedtime * 1.5,
      response_3mo == "Progression" ~ pmin(elapsedtime, 18),
      TRUE ~ elapsedtime
    ),
    outcome = case_when(
      response_3mo == "Progression" ~ 1,
      response_3mo == "Complete Response" & runif(n_landmark) < 0.3 ~ 0,
      TRUE ~ outcome
    )
  )

# Save Dataset 5
save(multisurvival_landmark, file = here("data", "multisurvival_landmark.rda"))
write.csv(multisurvival_landmark, file = here("data", "multisurvival_landmark.csv"), row.names = FALSE)
write_xlsx(multisurvival_landmark, path = here("data", "multisurvival_landmark.xlsx"))
jmvReadWrite::write_omv(multisurvival_landmark, here("data", "multisurvival_landmark.omv"))

# ───────────────────────────────────────────────────────────
# Dataset 6: Stratification Dataset (multisurvival_stratify)
# ───────────────────────────────────────────────────────────
# Data with proportional hazards violation requiring stratification

n_strat <- 150

multisurvival_stratify <- tibble(
  PatientID = paste0("ST", sprintf("%03d", 1:n_strat)),
  
  elapsedtime = pmax(1, rnorm(n_strat, mean = 36, sd = 24)),
  outcome = sample(c(0, 1), n_strat, replace = TRUE, prob = c(0.4, 0.6)),
  
  # Variables that may violate PH assumption
  # Sex has different hazard patterns over time
  sex = sample(c("Male", "Female"), n_strat, replace = TRUE),
  
  # Age categories
  age_category = sample(c("Young", "Middle", "Elderly"), 
                       n_strat, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
  
  # Other predictors
  treatment = sample(c("A", "B", "C"), n_strat, replace = TRUE),
  stage = sample(c("I-II", "III-IV"), n_strat, replace = TRUE),
  
  # Continuous
  age = round(rnorm(n_strat, 66, 13)),
  bmi = pmax(15, rnorm(n_strat, 26, 5))
)

# Create non-proportional hazards for sex
# Males have higher early hazard, females higher late hazard
multisurvival_stratify <- multisurvival_stratify %>%
  mutate(
    # Early period (< 12 months): males worse
    outcome = case_when(
      sex == "Male" & elapsedtime < 12 ~ ifelse(runif(n_strat) < 0.7, 1, outcome),
      # Late period (> 36 months): females worse  
      sex == "Female" & elapsedtime > 36 ~ ifelse(runif(n_strat) < 0.6, 1, outcome),
      TRUE ~ outcome
    )
  )

# Save Dataset 6
save(multisurvival_stratify, file = here("data", "multisurvival_stratify.rda"))
write.csv(multisurvival_stratify, file = here("data", "multisurvival_stratify.csv"), row.names = FALSE)
write_xlsx(multisurvival_stratify, path = here("data", "multisurvival_stratify.xlsx"))
jmvReadWrite::write_omv(multisurvival_stratify, here("data", "multisurvival_stratify.omv"))

# ───────────────────────────────────────────────────────────
# Dataset 7: Person-Time Dataset (multisurvival_persontime)
# ───────────────────────────────────────────────────────────
# For person-time and incidence rate calculations

n_pt <- 250

multisurvival_persontime <- tibble(
  PatientID = paste0("PT", sprintf("%03d", 1:n_pt)),
  
  # Wide range of follow-up times for interval analysis
  elapsedtime = pmax(1, c(
    runif(50, 1, 12),      # 0-12 months
    runif(60, 12, 36),     # 12-36 months  
    runif(70, 36, 60),     # 36-60 months
    runif(70, 60, 120)     # 60+ months
  )),
  
  # Events distributed across time intervals
  outcome = c(
    sample(c(0, 1), 50, replace = TRUE, prob = c(0.3, 0.7)),  # High early event rate
    sample(c(0, 1), 60, replace = TRUE, prob = c(0.5, 0.5)),
    sample(c(0, 1), 70, replace = TRUE, prob = c(0.6, 0.4)),
    sample(c(0, 1), 70, replace = TRUE, prob = c(0.7, 0.3))   # Lower late event rate
  ),
  
  # Exposure variables for rate calculations
  treatment_group = sample(c("Exposed", "Unexposed"), n_pt, replace = TRUE),
  age_group = sample(c("<50", "50-65", ">65"), n_pt, replace = TRUE,
                    prob = c(0.2, 0.5, 0.3)),
  risk_factor = sample(c("Present", "Absent"), n_pt, replace = TRUE,
                      prob = c(0.3, 0.7)),
  
  # Continuous
  age = round(rnorm(n_pt, 60, 15))
)

# Save Dataset 7
save(multisurvival_persontime, file = here("data", "multisurvival_persontime.rda"))
write.csv(multisurvival_persontime, file = here("data", "multisurvival_persontime.csv"), row.names = FALSE)
write_xlsx(multisurvival_persontime, path = here("data", "multisurvival_persontime.xlsx"))
jmvReadWrite::write_omv(multisurvival_persontime, here("data", "multisurvival_persontime.omv"))

# ───────────────────────────────────────────────────────────
# Dataset 8: Small Test Dataset (multisurvival_small)
# ───────────────────────────────────────────────────────────
# Minimal dataset for quick testing

n_small <- 30

multisurvival_small <- tibble(
  PatientID = paste0("SM", sprintf("%02d", 1:n_small)),
  
  elapsedtime = round(runif(n_small, 6, 60)),
  outcome = sample(c(0, 1), n_small, replace = TRUE),
  
  treatment = sample(c("A", "B"), n_small, replace = TRUE),
  stage = sample(c("Early", "Advanced"), n_small, replace = TRUE),
  
  age = round(rnorm(n_small, 65, 10))
)

# Save Dataset 8
save(multisurvival_small, file = here("data", "multisurvival_small.rda"))
write.csv(multisurvival_small, file = here("data", "multisurvival_small.csv"), row.names = FALSE)
write_xlsx(multisurvival_small, path = here("data", "multisurvival_small.xlsx"))
jmvReadWrite::write_omv(multisurvival_small, here("data", "multisurvival_small.omv"))

# ═══════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════

cat("
════════════════════════════════════════════════════════════
Test Data Generation Complete: multisurvival
════════════════════════════════════════════════════════════

Generated 8 comprehensive datasets:

1. multisurvival_test (n=200)
   - Standard multivariable Cox regression
   - Numeric elapsed time, binary outcome
   - Multiple categorical and continuous predictors
   - Realistic correlations (stage, nodes, treatment effects)

2. multisurvival_dates (n=150)
   - Date-based survival calculation
   - Diagnosis and follow-up dates (YYYY-MM-DD format)
   - Tests tint=TRUE, dxdate, fudate options
   - 2018-2023 date range

3. multisurvival_competing (n=180)
   - Competing risks analysis
   - Multiple outcome levels: Alive w/o Disease, Alive w Disease,
     Dead of Disease, Dead of Other
   - Tests analysistype: compete, multievent options

4. multisurvival_risk (n=200)
   - Risk stratification and scoring
   - Strong prognostic factors (stage, grade, subtype, ki67)
   - Clear risk group separation
   - Tests calculateRiskScore, numRiskGroups, plotRiskGroups

5. multisurvival_landmark (n=160)
   - Landmark time analysis
   - Early failures (< 3 months) and long-term survivors
   - Response at 3 months variable
   - Tests uselandmark, landmark options

6. multisurvival_stratify (n=150)
   - Proportional hazards violation
   - Non-proportional hazards for sex (time-varying effect)
   - Tests use_stratify, stratvar, ph_cox options

7. multisurvival_persontime (n=250)
   - Person-time metrics
   - Events distributed across time intervals (0-12, 12-36, 36-60, 60+)
   - Tests person_time, time_intervals, rate_multiplier

8. multisurvival_small (n=30)
   - Minimal dataset for quick testing
   - Basic variables only

Each dataset saved in 4 formats:
  ✓ RDA (R native)
  ✓ CSV (universal)
  ✓ XLSX (Excel)
  ✓ OMV (jamovi)

Total files generated: 32 (8 datasets × 4 formats)

Seed: 42 (for reproducibility)

════════════════════════════════════════════════════════════
")
