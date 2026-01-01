# Create Q-TWiST Test Datasets
# Quality-adjusted Time Without Symptoms or Toxicity Analysis
# Test data for oncology trials

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(tibble)

set.seed(2025)

# ========================================
# Dataset 1: Breast Cancer Adjuvant Trial
# ========================================
# Comparing Chemotherapy vs. Targeted Therapy
# Based on classic Gelber et al. trials

n_patients <- 400

qtwist_breast_cancer <- tibble(
  # Patient identifiers
  patient_id = 1:n_patients,
  site = factor(sample(paste0("Site_", LETTERS[1:8]), n_patients, replace = TRUE)),

  # Treatment allocation
  treatment = factor(
    rep(c("Standard Chemotherapy", "Targeted Therapy"), each = n_patients/2),
    levels = c("Standard Chemotherapy", "Targeted Therapy")
  ),

  # Baseline characteristics
  age = round(rnorm(n_patients, 58, 12)),
  stage = factor(
    sample(c("IIA", "IIB", "IIIA", "IIIB"), n_patients,
           replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
    levels = c("IIA", "IIB", "IIIA", "IIIB")
  ),
  er_status = factor(sample(c("Positive", "Negative"), n_patients,
                           replace = TRUE, prob = c(0.7, 0.3))),
  her2_status = factor(sample(c("Positive", "Negative"), n_patients,
                             replace = TRUE, prob = c(0.2, 0.8))),
  performance_status = factor(sample(0:2, n_patients,
                                    replace = TRUE, prob = c(0.6, 0.3, 0.1))),

  # Generate baseline hazard modifiers
  baseline_risk = rnorm(n_patients, 0, 0.3)
) %>%
  mutate(
    # Age effect
    age_effect = (age - 58) * 0.02,

    # Stage effect
    stage_effect = case_when(
      stage == "IIA" ~ 0,
      stage == "IIB" ~ 0.3,
      stage == "IIIA" ~ 0.5,
      stage == "IIIB" ~ 0.8
    ),

    # Treatment effect
    treatment_effect = ifelse(treatment == "Targeted Therapy", -0.35, 0),

    # Combined hazard
    log_hazard = baseline_risk + age_effect + stage_effect + treatment_effect
  )

# Generate survival times using exponential distribution
# Progression-Free Survival (PFS)
qtwist_breast_cancer <- qtwist_breast_cancer %>%
  mutate(
    # PFS: median ~14 months standard, ~18 months targeted
    pfs_hazard = exp(log_hazard + 0.05),
    time_pfs_months = rexp(n_patients, rate = pfs_hazard / 14),

    # Censoring for PFS (30% censored)
    pfs_censor_time = rexp(n_patients, rate = 0.02),

    # Observed PFS
    pfs_time_observed = pmin(time_pfs_months, pfs_censor_time),
    pfs_event_occurred = as.integer(time_pfs_months <= pfs_censor_time),
    pfs_event_factor = factor(
      pfs_event_occurred,
      levels = c(0, 1),
      labels = c("Censored", "Progression or Death")
    )
  )

# Overall Survival (OS)
qtwist_breast_cancer <- qtwist_breast_cancer %>%
  mutate(
    # OS: median ~22 months standard, ~28 months targeted
    os_hazard = exp(log_hazard),
    time_os_months = rexp(n_patients, rate = os_hazard / 24),

    # Ensure OS >= PFS
    time_os_months = pmax(time_os_months, pfs_time_observed + rexp(n_patients, 0.1)),

    # Censoring for OS (40% censored)
    os_censor_time = rexp(n_patients, rate = 0.015),

    # Observed OS
    os_time_observed = pmin(time_os_months, os_censor_time),
    os_event_occurred = as.integer(time_os_months <= os_censor_time),
    os_event_factor = factor(
      os_event_occurred,
      levels = c(0, 1),
      labels = c("Alive", "Dead")
    )
  )

# Toxicity data
qtwist_breast_cancer <- qtwist_breast_cancer %>%
  mutate(
    # Grade 3-4 toxicity in first 6 months
    # Standard chemo: higher toxicity
    # Targeted therapy: lower but longer toxicity

    # Did patient have grade 3-4 toxicity?
    had_grade34_toxicity = rbinom(
      n_patients, 1,
      ifelse(treatment == "Standard Chemotherapy", 0.65, 0.35)
    ),

    # Time to first grade 3-4 toxicity (if it occurred)
    time_to_toxicity = ifelse(
      had_grade34_toxicity == 1,
      rexp(n_patients, rate = ifelse(treatment == "Standard Chemotherapy", 8, 12)),
      NA
    ),

    # Duration of grade 3-4 toxicity (days)
    toxicity_duration_days = ifelse(
      had_grade34_toxicity == 1,
      pmax(7, rnorm(
        n_patients,
        mean = ifelse(treatment == "Standard Chemotherapy", 45, 30),
        sd = 20
      )),
      0
    ),

    # Convert to months
    toxicity_duration_months = toxicity_duration_days / 30.44,

    # Time to toxicity end
    time_toxicity_end = ifelse(
      had_grade34_toxicity == 1,
      time_to_toxicity + toxicity_duration_months,
      NA
    ),

    # Cap toxicity at 6 months (typical assessment window)
    toxicity_window_months = pmin(toxicity_duration_months, 6, na.rm = TRUE),

    # Toxicity status factor
    toxicity_status = factor(
      had_grade34_toxicity,
      levels = c(0, 1),
      labels = c("No Grade 3-4", "Grade 3-4 Toxicity")
    )
  )

# Clean up intermediate calculation columns
qtwist_breast_cancer_final <- qtwist_breast_cancer %>%
  select(
    # Identifiers
    patient_id, site, treatment,

    # Baseline characteristics
    age, stage, er_status, her2_status, performance_status,

    # Overall Survival
    os_time_observed, os_event_occurred, os_event_factor,

    # Progression-Free Survival
    pfs_time_observed, pfs_event_occurred, pfs_event_factor,

    # Toxicity
    had_grade34_toxicity, toxicity_status,
    time_to_toxicity, toxicity_duration_days, toxicity_duration_months,
    time_toxicity_end, toxicity_window_months
  ) %>%
  rename(
    # Make variable names clear for jamovi
    overall_survival_months = os_time_observed,
    death_event = os_event_occurred,
    death_status = os_event_factor,

    progression_free_months = pfs_time_observed,
    progression_event = pfs_event_occurred,
    progression_status = pfs_event_factor,

    toxicity_grade34 = had_grade34_toxicity
  )

# Save dataset
qtwist_breast_cancer <- qtwist_breast_cancer_final
use_data_multi_format(qtwist_breast_cancer, overwrite = TRUE, save_csv = TRUE)


# ========================================
# Dataset 2: Lung Cancer Trial (Simpler)
# ========================================
# For basic Q-TWiST demonstration

n_lung <- 200

qtwist_lung_simple <- tibble(
  patient_id = 1:n_lung,

  # Treatment
  arm = factor(
    rep(c("Control", "Experimental"), each = n_lung/2),
    levels = c("Control", "Experimental")
  ),

  # Baseline
  age = round(rnorm(n_lung, 65, 10)),
  sex = factor(sample(c("Male", "Female"), n_lung, replace = TRUE, prob = c(0.6, 0.4))),

  # Simple survival times
  # Control: median OS = 12 months, PFS = 6 months
  # Experimental: median OS = 18 months, PFS = 10 months

  os_months = rexp(n_lung, rate = ifelse(arm == "Experimental", 1/18, 1/12)),
  pfs_months = rexp(n_lung, rate = ifelse(arm == "Experimental", 1/10, 1/6)),

  # Ensure PFS <= OS
  pfs_months = pmin(pfs_months, os_months - 0.5),

  # Events (70% for PFS, 60% for OS)
  pfs_event = rbinom(n_lung, 1, 0.7),
  os_event = rbinom(n_lung, 1, 0.6),

  # Ensure if OS event, then PFS event
  pfs_event = pmax(pfs_event, os_event),

  # Toxicity: first 3 months, more in experimental
  tox_months = ifelse(
    rbinom(n_lung, 1, ifelse(arm == "Experimental", 0.5, 0.3)) == 1,
    runif(n_lung, 0.5, 3),
    0
  )
) %>%
  # Add factors
  mutate(
    os_status = factor(os_event, levels = c(0, 1), labels = c("Censored", "Death")),
    pfs_status = factor(pfs_event, levels = c(0, 1), labels = c("Censored", "Event"))
  )

# Save
use_data_multi_format(qtwist_lung_simple, overwrite = TRUE, save_csv = TRUE)


# ========================================
# Dataset 3: Colorectal Cancer with Detailed Toxicity
# ========================================

n_crc <- 300

qtwist_colorectal <- tibble(
  id = 1:n_crc,
  treatment_arm = factor(
    sample(c("FOLFOX", "FOLFIRI", "Bevacizumab + FOLFOX"), n_crc, replace = TRUE),
    levels = c("FOLFOX", "FOLFIRI", "Bevacizumab + FOLFOX")
  ),

  # Patient characteristics
  age = round(rnorm(n_crc, 62, 11)),
  kras_mutation = factor(sample(c("Wild-type", "Mutant"), n_crc, replace = TRUE, prob = c(0.6, 0.4))),
  primary_site = factor(sample(c("Right colon", "Left colon", "Rectum"), n_crc, replace = TRUE)),

  # Survival outcomes
  os_time = pmax(3, rnorm(n_crc,
                          mean = case_when(
                            treatment_arm == "FOLFOX" ~ 20,
                            treatment_arm == "FOLFIRI" ~ 19,
                            treatment_arm == "Bevacizumab + FOLFOX" ~ 24
                          ),
                          sd = 8)),

  pfs_time = pmax(2, rnorm(n_crc,
                          mean = case_when(
                            treatment_arm == "FOLFOX" ~ 9,
                            treatment_arm == "FOLFIRI" ~ 8.5,
                            treatment_arm == "Bevacizumab + FOLFOX" ~ 11
                          ),
                          sd = 4)),

  # Events
  death = rbinom(n_crc, 1, 0.65),
  progression = rbinom(n_crc, 1, 0.80),

  # Detailed toxicity tracking
  # Neuropathy (mainly FOLFOX)
  neuropathy_grade = case_when(
    treatment_arm == "FOLFOX" ~ sample(0:3, n_crc, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
    treatment_arm == "Bevacizumab + FOLFOX" ~ sample(0:3, n_crc, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
    TRUE ~ sample(0:2, n_crc, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  ),

  # Diarrhea (mainly FOLFIRI)
  diarrhea_grade = case_when(
    treatment_arm == "FOLFIRI" ~ sample(0:4, n_crc, replace = TRUE, prob = c(0.2, 0.2, 0.3, 0.2, 0.1)),
    TRUE ~ sample(0:3, n_crc, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))
  ),

  # Any grade 3-4 toxicity
  any_grade34 = as.integer(neuropathy_grade >= 3 | diarrhea_grade >= 3),

  # Toxicity duration
  tox_duration = ifelse(
    any_grade34 == 1,
    rexp(n_crc, rate = 1/2) * 30.44, # days
    0
  )
) %>%
  # Ensure logical consistency
  mutate(
    pfs_time = pmin(pfs_time, os_time - 0.1),
    progression = pmax(progression, death),

    # Factors
    death_status = factor(death, levels = c(0, 1), labels = c("Alive", "Deceased")),
    progression_status = factor(progression, levels = c(0, 1), labels = c("No Event", "Progression/Death")),
    toxicity_any34 = factor(any_grade34, levels = c(0, 1), labels = c("Grade 0-2", "Grade 3-4"))
  )

# Save
use_data_multi_format(qtwist_colorectal, overwrite = TRUE, save_csv = TRUE)

# ========================================
# Document datasets
# ========================================

# Create documentation file
cat('
#\' Q-TWiST Test Datasets
#\'
#\' @description
#\' Example datasets for Q-TWiST (Quality-adjusted Time Without Symptoms or Toxicity) analysis.
#\' These datasets represent realistic oncology clinical trial data with overall survival,
#\' progression-free survival, and toxicity information.
#\'
#\' @format
#\' ## qtwist_breast_cancer
#\' A data frame with 400 patients from a breast cancer adjuvant therapy trial:
#\' \\describe{
#\'   \\item{patient_id}{Unique patient identifier}
#\'   \\item{site}{Clinical trial site}
#\'   \\item{treatment}{Treatment arm (Standard Chemotherapy vs Targeted Therapy)}
#\'   \\item{age}{Age in years}
#\'   \\item{stage}{Disease stage (IIA, IIB, IIIA, IIIB)}
#\'   \\item{er_status}{Estrogen receptor status}
#\'   \\item{her2_status}{HER2 receptor status}
#\'   \\item{performance_status}{ECOG performance status (0-2)}
#\'   \\item{overall_survival_months}{Time to death or censoring (months)}
#\'   \\item{death_event}{Death event indicator (0=censored, 1=death)}
#\'   \\item{death_status}{Death status factor}
#\'   \\item{progression_free_months}{Time to progression/death or censoring (months)}
#\'   \\item{progression_event}{Progression event indicator}
#\'   \\item{progression_status}{Progression status factor}
#\'   \\item{toxicity_grade34}{Indicator for grade 3-4 toxicity}
#\'   \\item{toxicity_status}{Toxicity status factor}
#\'   \\item{time_to_toxicity}{Time to first grade 3-4 toxicity (months)}
#\'   \\item{toxicity_duration_days}{Duration of grade 3-4 toxicity (days)}
#\'   \\item{toxicity_duration_months}{Duration of toxicity (months)}
#\'   \\item{time_toxicity_end}{Time when toxicity resolved (months)}
#\'   \\item{toxicity_window_months}{Toxicity duration capped at 6 months}
#\' }
#\'
#\' ## qtwist_lung_simple
#\' A simplified data frame with 200 lung cancer patients:
#\' \\describe{
#\'   \\item{patient_id}{Patient ID}
#\'   \\item{arm}{Treatment arm (Control vs Experimental)}
#\'   \\item{age}{Age in years}
#\'   \\item{sex}{Patient sex}
#\'   \\item{os_months}{Overall survival time (months)}
#\'   \\item{pfs_months}{Progression-free survival time (months)}
#\'   \\item{pfs_event}{PFS event indicator}
#\'   \\item{os_event}{OS event indicator}
#\'   \\item{tox_months}{Duration of toxicity (months)}
#\'   \\item{os_status}{OS status factor}
#\'   \\item{pfs_status}{PFS status factor}
#\' }
#\'
#\' ## qtwist_colorectal
#\' A data frame with 300 colorectal cancer patients across 3 treatment arms:
#\' \\describe{
#\'   \\item{id}{Patient identifier}
#\'   \\item{treatment_arm}{Treatment regimen (FOLFOX, FOLFIRI, Bevacizumab + FOLFOX)}
#\'   \\item{age}{Age in years}
#\'   \\item{kras_mutation}{KRAS mutation status}
#\'   \\item{primary_site}{Primary tumor location}
#\'   \\item{os_time}{Overall survival time (months)}
#\'   \\item{pfs_time}{Progression-free survival time (months)}
#\'   \\item{death}{Death event indicator}
#\'   \\item{progression}{Progression event indicator}
#\'   \\item{neuropathy_grade}{Neuropathy grade (0-3)}
#\'   \\item{diarrhea_grade}{Diarrhea grade (0-4)}
#\'   \\item{any_grade34}{Any grade 3-4 toxicity indicator}
#\'   \\item{tox_duration}{Toxicity duration (days)}
#\'   \\item{death_status}{Death status factor}
#\'   \\item{progression_status}{Progression status factor}
#\'   \\item{toxicity_any34}{Toxicity grade factor}
#\' }
#\'
#\' @source Generated based on published oncology clinical trial results
#\' @references
#\' Gelber RD, Goldhirsch A (1986). A new endpoint for the assessment of adjuvant
#\' therapy in postmenopausal women with operable breast cancer. J Clin Oncol, 4(12):1772-1779.
#\'
#\' @examples
#\' # Load breast cancer dataset
#\' data("qtwist_breast_cancer")
#\'
#\' # Basic summary
#\' summary(qtwist_breast_cancer)
#\'
#\' # View treatment distribution
#\' table(qtwist_breast_cancer$treatment)
#\'
#\' # Median survival by treatment
#\' tapply(qtwist_breast_cancer$overall_survival_months,
#\'        qtwist_breast_cancer$treatment,
#\'        median)
#\'
#\' @name qtwist_datasets
"qtwist_breast_cancer"

#\' @rdname qtwist_datasets
"qtwist_lung_simple"

#\' @rdname qtwist_datasets
"qtwist_colorectal"
', file = "R/data_qtwist_docs.R")

message("✅ Q-TWiST test datasets created successfully!")
message("   - qtwist_breast_cancer: 400 patients, detailed toxicity tracking")
message("   - qtwist_lung_simple: 200 patients, simplified structure")
message("   - qtwist_colorectal: 300 patients, 3-arm trial with specific toxicities")
