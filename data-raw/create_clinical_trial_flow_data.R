## ---- Create Comprehensive Clinical Trial Flow Datasets ----
## Purpose: Generate realistic datasets for testing consortdiagram and studydiagram
## Created: 2025-10-05
## Functions tested: consortdiagram, studydiagram

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(20251005)

# ============================================================================
# DATASET 1: RCT with Multiple Exclusion Stages (for consortdiagram)
# ============================================================================
# Scenario: Phase III randomized controlled trial for cancer treatment
# Arms: Treatment A vs Treatment B
# Stages: Screening -> Enrollment -> Randomization -> Allocation -> Follow-up -> Analysis

cat("Generating clinical_trial_consort_data...\n")

n_assessed <- 500  # Participants assessed for eligibility

# Create participant IDs
participant_ids <- sprintf("PT-%04d", 1:n_assessed)

# Initialize dataset
clinical_trial_consort_data <- data.frame(
  participant_id = participant_ids,
  age = round(rnorm(n_assessed, mean = 62, sd = 12)),
  sex = sample(c("Male", "Female"), n_assessed, replace = TRUE, prob = c(0.6, 0.4)),
  stringsAsFactors = FALSE
)

# STAGE 1: Screening Exclusions (20% excluded)
# Reasons: Age criteria, performance status, comorbidities
screening_exclusion_rate <- 0.20
n_screening_excluded <- round(n_assessed * screening_exclusion_rate)

screening_reasons <- c(
  "Age < 18 years",
  "Age > 80 years",
  "ECOG PS > 2",
  "Severe comorbidity",
  "Prior malignancy",
  "Inadequate organ function"
)

clinical_trial_consort_data$age_exclusion <- NA
clinical_trial_consort_data$performance_exclusion <- NA
clinical_trial_consort_data$comorbidity_exclusion <- NA

# Assign screening exclusions
screening_excluded_idx <- sample(1:n_assessed, n_screening_excluded)

for (idx in screening_excluded_idx) {
  reason <- sample(screening_reasons, 1)

  if (grepl("Age", reason)) {
    clinical_trial_consort_data$age_exclusion[idx] <- reason
  } else if (grepl("ECOG|organ", reason)) {
    clinical_trial_consort_data$performance_exclusion[idx] <- reason
  } else {
    clinical_trial_consort_data$comorbidity_exclusion[idx] <- reason
  }
}

# Remaining after screening
remaining_idx <- setdiff(1:n_assessed, screening_excluded_idx)
n_remaining <- length(remaining_idx)

# STAGE 2: Enrollment Exclusions (10% of remaining)
# Reasons: Consent refusal, eligibility re-assessment, protocol violation
enrollment_exclusion_rate <- 0.10
n_enrollment_excluded <- round(n_remaining * enrollment_exclusion_rate)

enrollment_reasons <- c(
  "Consent refused",
  "Consent withdrawn",
  "Lost before enrollment",
  "Eligibility criteria not met",
  "Patient decision"
)

clinical_trial_consort_data$consent_exclusion <- NA
clinical_trial_consort_data$eligibility_exclusion <- NA

enrollment_excluded_idx <- sample(remaining_idx, n_enrollment_excluded)

for (idx in enrollment_excluded_idx) {
  reason <- sample(enrollment_reasons, 1)

  if (grepl("Consent|decision", reason)) {
    clinical_trial_consort_data$consent_exclusion[idx] <- reason
  } else {
    clinical_trial_consort_data$eligibility_exclusion[idx] <- reason
  }
}

# Remaining after enrollment
remaining_idx <- setdiff(remaining_idx, enrollment_excluded_idx)
n_randomized <- length(remaining_idx)

# RANDOMIZATION: Assign to treatment arms
clinical_trial_consort_data$treatment_arm <- NA
clinical_trial_consort_data$treatment_arm[remaining_idx] <- sample(
  c("Treatment A", "Treatment B"),
  n_randomized,
  replace = TRUE
)

# STAGE 3: Post-Randomization/Allocation Exclusions (5% of randomized)
# Reasons: Did not receive allocated intervention
allocation_exclusion_rate <- 0.05
n_allocation_excluded <- round(n_randomized * allocation_exclusion_rate)

allocation_reasons <- c(
  "Intervention not received - patient refusal",
  "Intervention not received - clinical deterioration",
  "Intervention not received - protocol deviation",
  "Withdrew before treatment",
  "Ineligible after randomization"
)

clinical_trial_consort_data$allocation_exclusion <- NA

allocation_excluded_idx <- sample(remaining_idx, n_allocation_excluded)

for (idx in allocation_excluded_idx) {
  clinical_trial_consort_data$allocation_exclusion[idx] <- sample(allocation_reasons, 1)
}

# Remaining after allocation
remaining_idx <- setdiff(remaining_idx, allocation_excluded_idx)
n_received_intervention <- length(remaining_idx)

# STAGE 4: Follow-up Exclusions (12% of those who received intervention)
# Reasons: Lost to follow-up, death, withdrawal
followup_exclusion_rate <- 0.12
n_followup_excluded <- round(n_received_intervention * followup_exclusion_rate)

followup_reasons <- c(
  "Lost to follow-up",
  "Death unrelated to disease",
  "Withdrew consent during follow-up",
  "Moved to different institution",
  "Unable to contact"
)

clinical_trial_consort_data$followup_loss <- NA
clinical_trial_consort_data$followup_death <- NA

followup_excluded_idx <- sample(remaining_idx, n_followup_excluded)

for (idx in followup_excluded_idx) {
  reason <- sample(followup_reasons, 1)

  if (grepl("Lost|contact|Moved", reason)) {
    clinical_trial_consort_data$followup_loss[idx] <- reason
  } else {
    clinical_trial_consort_data$followup_death[idx] <- reason
  }
}

# Remaining after follow-up
remaining_idx <- setdiff(remaining_idx, followup_excluded_idx)
n_completed_followup <- length(remaining_idx)

# STAGE 5: Analysis Exclusions (3% of those who completed follow-up)
# Reasons: Missing outcome data, protocol violations
analysis_exclusion_rate <- 0.03
n_analysis_excluded <- round(n_completed_followup * analysis_exclusion_rate)

analysis_reasons <- c(
  "Missing outcome data",
  "Protocol violation",
  "Major protocol deviation",
  "Incomplete data"
)

clinical_trial_consort_data$analysis_exclusion <- NA

analysis_excluded_idx <- sample(remaining_idx, n_analysis_excluded)

for (idx in analysis_excluded_idx) {
  clinical_trial_consort_data$analysis_exclusion[idx] <- sample(analysis_reasons, 1)
}

# Final analyzed participants
remaining_idx <- setdiff(remaining_idx, analysis_excluded_idx)
n_analyzed <- length(remaining_idx)

# Add outcome variable for completeness
clinical_trial_consort_data$outcome_response <- NA
clinical_trial_consort_data$outcome_response[remaining_idx] <- sample(
  c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"),
  n_analyzed,
  replace = TRUE,
  prob = c(0.15, 0.35, 0.30, 0.20)
)

# Summary statistics
cat("\n=== CONSORT Data Summary ===\n")
cat("Total assessed:", n_assessed, "\n")
cat("Screening excluded:", n_screening_excluded, sprintf("(%.1f%%)\n", screening_exclusion_rate * 100))
cat("Enrollment excluded:", n_enrollment_excluded, sprintf("(%.1f%%)\n", enrollment_exclusion_rate * 100))
cat("Randomized:", n_randomized, "\n")
cat("  Treatment A:", sum(clinical_trial_consort_data$treatment_arm == "Treatment A", na.rm = TRUE), "\n")
cat("  Treatment B:", sum(clinical_trial_consort_data$treatment_arm == "Treatment B", na.rm = TRUE), "\n")
cat("Allocation excluded:", n_allocation_excluded, sprintf("(%.1f%%)\n", allocation_exclusion_rate * 100))
cat("Follow-up excluded:", n_followup_excluded, sprintf("(%.1f%%)\n", followup_exclusion_rate * 100))
cat("Analysis excluded:", n_analysis_excluded, sprintf("(%.1f%%)\n", analysis_exclusion_rate * 100))
cat("Final analyzed:", n_analyzed, sprintf("(%.1f%% retention)\n", n_analyzed / n_assessed * 100))

# Save as .rda
save(clinical_trial_consort_data, file = "data/clinical_trial_consort_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(clinical_trial_consort_data, "data/clinical_trial_consort_data.omv")
  message("✓ Created clinical_trial_consort_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(clinical_trial_consort_data, "data/clinical_trial_consort_data.omv")
  message("✓ Created clinical_trial_consort_data.omv")
}
cat("\n✓ Saved: data/clinical_trial_consort_data.rda\n")

# Save as .csv
write.csv(clinical_trial_consort_data,
          file = "data/clinical_trial_consort_data.csv",
          row.names = FALSE)
cat("✓ Saved: data/clinical_trial_consort_data.csv\n")


# ============================================================================
# DATASET 2: Observational Study Flow (for studydiagram)
# ============================================================================
# Scenario: Prospective cohort study without randomization
# Focus: Study flow visualization with multiple enrollment pathways

cat("\n\nGenerating observational_study_flow_data...\n")

n_participants <- 350

# Create participant IDs
obs_participant_ids <- sprintf("OBS-%04d", 1:n_participants)

observational_study_flow_data <- data.frame(
  participant_id = obs_participant_ids,
  enrollment_date = seq(as.Date("2023-01-01"), by = "day", length.out = n_participants),
  age = round(rnorm(n_participants, mean = 58, sd = 15)),
  diagnosis = sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                    n_participants,
                    replace = TRUE,
                    prob = c(0.25, 0.35, 0.25, 0.15)),
  stringsAsFactors = FALSE
)

# Multiple exclusion pathways

# Initial screening (15% excluded)
n_initial_excluded <- round(n_participants * 0.15)
initial_excluded_idx <- sample(1:n_participants, n_initial_excluded)

observational_study_flow_data$initial_exclusion <- NA
observational_study_flow_data$initial_exclusion[initial_excluded_idx] <- sample(
  c("Incomplete records", "Duplicate entry", "Outside study period"),
  n_initial_excluded,
  replace = TRUE
)

# Remaining participants
remaining_idx <- setdiff(1:n_participants, initial_excluded_idx)

# Consent/enrollment (8% excluded)
n_consent_excluded <- round(length(remaining_idx) * 0.08)
consent_excluded_idx <- sample(remaining_idx, n_consent_excluded)

observational_study_flow_data$consent_status <- NA
observational_study_flow_data$consent_status[consent_excluded_idx] <- sample(
  c("Declined participation", "Unable to contact"),
  n_consent_excluded,
  replace = TRUE
)

# Remaining participants
remaining_idx <- setdiff(remaining_idx, consent_excluded_idx)

# Follow-up completion (10% incomplete)
n_followup_incomplete <- round(length(remaining_idx) * 0.10)
followup_incomplete_idx <- sample(remaining_idx, n_followup_incomplete)

observational_study_flow_data$followup_status <- NA
observational_study_flow_data$followup_status[followup_incomplete_idx] <- sample(
  c("Lost to follow-up", "Moved", "Death", "Withdrew"),
  n_followup_incomplete,
  replace = TRUE
)

# Final analysis set
remaining_idx <- setdiff(remaining_idx, followup_incomplete_idx)
n_final <- length(remaining_idx)

observational_study_flow_data$in_final_analysis <- FALSE
observational_study_flow_data$in_final_analysis[remaining_idx] <- TRUE

# Add study group (not randomized, observational)
observational_study_flow_data$study_group <- NA
observational_study_flow_data$study_group[remaining_idx] <- sample(
  c("Cohort A - Surgery", "Cohort B - Radiation", "Cohort C - Combination"),
  n_final,
  replace = TRUE
)

# Summary
cat("\n=== Observational Study Summary ===\n")
cat("Total enrolled:", n_participants, "\n")
cat("Initial excluded:", n_initial_excluded, "\n")
cat("Consent excluded:", n_consent_excluded, "\n")
cat("Follow-up incomplete:", n_followup_incomplete, "\n")
cat("Final analysis:", n_final, sprintf("(%.1f%% retention)\n", n_final / n_participants * 100))

# Save as .rda
save(observational_study_flow_data, file = "data/observational_study_flow_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(observational_study_flow_data, "data/observational_study_flow_data.omv")
  message("✓ Created observational_study_flow_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(observational_study_flow_data, "data/observational_study_flow_data.omv")
  message("✓ Created observational_study_flow_data.omv")
}
cat("\n✓ Saved: data/observational_study_flow_data.rda\n")

# Save as .csv
write.csv(observational_study_flow_data,
          file = "data/observational_study_flow_data.csv",
          row.names = FALSE)
cat("✓ Saved: data/observational_study_flow_data.csv\n")


# ============================================================================
# DATASET 3: Multi-center Trial with Complex Flow (for consortdiagram)
# ============================================================================
# Scenario: Multi-center trial with differential retention by site

cat("\n\nGenerating multicenter_trial_data...\n")

n_total <- 600
sites <- c("Site A", "Site B", "Site C", "Site D")

multicenter_trial_data <- data.frame(
  participant_id = sprintf("MC-%05d", 1:n_total),
  site = sample(sites, n_total, replace = TRUE),
  age = round(rnorm(n_total, mean = 65, sd = 10)),
  sex = sample(c("Male", "Female"), n_total, replace = TRUE),
  stringsAsFactors = FALSE
)

# Screening exclusions (25% - higher rate for realism)
n_screen_excl <- round(n_total * 0.25)
screen_excl_idx <- sample(1:n_total, n_screen_excl)

multicenter_trial_data$screening_failure <- NA
multicenter_trial_data$screening_failure[screen_excl_idx] <- sample(
  c("Inclusion criteria not met", "Exclusion criteria present", "Lab values out of range"),
  n_screen_excl,
  replace = TRUE
)

remaining_idx <- setdiff(1:n_total, screen_excl_idx)

# Enrollment (5% decline)
n_enroll_excl <- round(length(remaining_idx) * 0.05)
enroll_excl_idx <- sample(remaining_idx, n_enroll_excl)

multicenter_trial_data$enrollment_issue <- NA
multicenter_trial_data$enrollment_issue[enroll_excl_idx] <- sample(
  c("Refused consent", "Travel distance too far"),
  n_enroll_excl,
  replace = TRUE
)

remaining_idx <- setdiff(remaining_idx, enroll_excl_idx)
n_randomized_mc <- length(remaining_idx)

# Randomization
multicenter_trial_data$arm <- NA
multicenter_trial_data$arm[remaining_idx] <- sample(
  c("Experimental", "Control"),
  n_randomized_mc,
  replace = TRUE
)

# Allocation (3%)
n_alloc_excl <- round(n_randomized_mc * 0.03)
alloc_excl_idx <- sample(remaining_idx, n_alloc_excl)

multicenter_trial_data$not_received <- NA
multicenter_trial_data$not_received[alloc_excl_idx] <- sample(
  c("Intervention not available", "Patient deteriorated"),
  n_alloc_excl,
  replace = TRUE
)

remaining_idx <- setdiff(remaining_idx, alloc_excl_idx)

# Follow-up (15% loss - realistic for multi-center)
n_fu_loss <- round(length(remaining_idx) * 0.15)
fu_loss_idx <- sample(remaining_idx, n_fu_loss)

multicenter_trial_data$followup_loss_reason <- NA
multicenter_trial_data$followup_loss_reason[fu_loss_idx] <- sample(
  c("Lost to follow-up", "Withdrew consent", "Site closure"),
  n_fu_loss,
  replace = TRUE
)

remaining_idx <- setdiff(remaining_idx, fu_loss_idx)

# Analysis exclusion (2%)
n_analysis_excl_mc <- round(length(remaining_idx) * 0.02)
analysis_excl_idx <- sample(remaining_idx, n_analysis_excl_mc)

multicenter_trial_data$analysis_issue <- NA
multicenter_trial_data$analysis_issue[analysis_excl_idx] <- "Missing primary endpoint"

remaining_idx <- setdiff(remaining_idx, analysis_excl_idx)
n_final_mc <- length(remaining_idx)

cat("\n=== Multi-center Trial Summary ===\n")
cat("Total assessed:", n_total, "\n")
cat("Final analyzed:", n_final_mc, sprintf("(%.1f%% retention)\n", n_final_mc / n_total * 100))
cat("By site:\n")
for (site in sites) {
  site_total <- sum(multicenter_trial_data$site == site)
  site_final <- sum(multicenter_trial_data$site == site & multicenter_trial_data$participant_id %in% multicenter_trial_data$participant_id[remaining_idx], na.rm = TRUE)
  cat(sprintf("  %s: %d/%d (%.1f%%)\n", site, site_final, site_total, site_final/site_total * 100))
}

# Save
save(multicenter_trial_data, file = "data/multicenter_trial_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(multicenter_trial_data, "data/multicenter_trial_data.omv")
  message("✓ Created multicenter_trial_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(multicenter_trial_data, "data/multicenter_trial_data.omv")
  message("✓ Created multicenter_trial_data.omv")
}
cat("\n✓ Saved: data/multicenter_trial_data.rda\n")

write.csv(multicenter_trial_data,
          file = "data/multicenter_trial_data.csv",
          row.names = FALSE)
cat("✓ Saved: data/multicenter_trial_data.csv\n")


# ============================================================================
# Create documentation file
# ============================================================================

documentation <- "
# Clinical Trial Flow Datasets

## Generated: 2025-10-05

This file documents the datasets created for testing `consortdiagram` and `studydiagram` functions.

---

## 1. clinical_trial_consort_data

**Purpose**: Test all features of `consortdiagram` with realistic RCT data

**Design**: Phase III randomized controlled trial (2-arm)

**Sample Size**: 500 participants assessed

**Treatment Arms**:
- Treatment A
- Treatment B

**Exclusion Stages**:
1. **Screening** (20% excluded):
   - Variables: `age_exclusion`, `performance_exclusion`, `comorbidity_exclusion`
   - Reasons: Age criteria, ECOG PS, comorbidities, prior malignancy

2. **Enrollment** (10% of remaining):
   - Variables: `consent_exclusion`, `eligibility_exclusion`
   - Reasons: Consent refusal, eligibility re-assessment

3. **Allocation** (5% of randomized):
   - Variable: `allocation_exclusion`
   - Reasons: Intervention not received, protocol deviation

4. **Follow-up** (12% of allocated):
   - Variables: `followup_loss`, `followup_death`
   - Reasons: Lost to follow-up, death, withdrawal

5. **Analysis** (3% of completed):
   - Variable: `analysis_exclusion`
   - Reasons: Missing data, protocol violations

**Final Retention**: ~60% (realistic for complex trials)

**Key Variables**:
- `participant_id`: Unique identifier (PT-0001 to PT-0500)
- `treatment_arm`: Treatment A or Treatment B
- `age`, `sex`: Demographics
- `outcome_response`: Treatment outcome (for analyzed participants)

---

## 2. observational_study_flow_data

**Purpose**: Test `studydiagram` with observational study design

**Design**: Prospective cohort study (non-randomized)

**Sample Size**: 350 participants

**Study Groups** (observational allocation):
- Cohort A - Surgery
- Cohort B - Radiation
- Cohort C - Combination

**Exclusion Stages**:
1. **Initial Screening** (15%):
   - Variable: `initial_exclusion`
   - Reasons: Incomplete records, duplicates

2. **Consent** (8%):
   - Variable: `consent_status`
   - Reasons: Declined, unable to contact

3. **Follow-up** (10%):
   - Variable: `followup_status`
   - Reasons: Lost, moved, death, withdrew

**Final Retention**: ~70%

**Key Variables**:
- `participant_id`: OBS-0001 to OBS-0350
- `enrollment_date`: Enrollment dates
- `diagnosis`: Disease stage
- `study_group`: Observational cohort assignment
- `in_final_analysis`: Boolean flag

---

## 3. multicenter_trial_data

**Purpose**: Test complex multi-site scenarios

**Design**: Multi-center RCT (4 sites)

**Sample Size**: 600 participants

**Sites**: Site A, Site B, Site C, Site D

**Treatment Arms**: Experimental vs Control

**Exclusion Stages** (higher attrition for realism):
1. Screening (25%)
2. Enrollment (5%)
3. Allocation (3%)
4. Follow-up (15% - site variability)
5. Analysis (2%)

**Final Retention**: ~55% (realistic for multi-center)

**Key Variables**:
- `participant_id`: MC-00001 to MC-00600
- `site`: Study site
- `arm`: Experimental or Control
- Multiple exclusion reason variables

---

## Usage Examples

### Testing consortdiagram:

```r
# Load data
load('data/clinical_trial_consort_data.rda')

# In jamovi:
# 1. Load clinical_trial_consort_data
# 2. Select participant_id
# 3. Screening: age_exclusion, performance_exclusion, comorbidity_exclusion
# 4. Enrollment: consent_exclusion, eligibility_exclusion
# 5. Randomization: treatment_arm
# 6. Allocation: allocation_exclusion
# 7. Follow-up: followup_loss, followup_death
# 8. Analysis: analysis_exclusion
# 9. Enable 'Show detailed exclusion reasons'
# 10. Enable 'Include clinical interpretation'
```

### Testing studydiagram:

```r
# Load data
load('data/observational_study_flow_data.rda')

# In jamovi:
# 1. Load observational_study_flow_data
# 2. Select participant_id
# 3. Add exclusion stages
# 4. Customize labels
```

---

## Data Quality Notes

- All exclusion variables use NA for participants who continued to the next stage
- Non-NA values represent exclusion at that specific stage
- Exclusions are cumulative (once excluded, participant doesn't appear in later stages)
- Realistic retention rates based on published clinical trial data
- Multiple exclusion reasons per stage for comprehensive testing

---

## Files Generated

- `data/clinical_trial_consort_data.rda`
- `data/clinical_trial_consort_data.csv`
- `data/observational_study_flow_data.rda`
- `data/observational_study_flow_data.csv`
- `data/multicenter_trial_data.rda`
- `data/multicenter_trial_data.csv`

---

## Maintenance

To regenerate these datasets:

```r
source('data-raw/create_clinical_trial_flow_data.R')
```

Seed: 20251005 (for reproducibility)
"

# Save documentation
writeLines(documentation, "data-raw/CLINICAL_TRIAL_FLOW_DATA_README.md")
cat("\n✓ Saved: data-raw/CLINICAL_TRIAL_FLOW_DATA_README.md\n")

cat("\n========================================\n")
cat("✅ ALL DATASETS CREATED SUCCESSFULLY\n")
cat("========================================\n")
cat("\nFiles created:\n")
cat("  - data/clinical_trial_consort_data.rda/.csv\n")
cat("  - data/observational_study_flow_data.rda/.csv\n")
cat("  - data/multicenter_trial_data.rda/.csv\n")
cat("  - data-raw/CLINICAL_TRIAL_FLOW_DATA_README.md\n")
cat("\nReady for testing!\n\n")
