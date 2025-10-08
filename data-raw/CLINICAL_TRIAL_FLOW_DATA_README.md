
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

