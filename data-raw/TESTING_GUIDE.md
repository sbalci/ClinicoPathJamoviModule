# Testing Guide for CONSORT and Study Diagrams

## Quick Start: Testing in jamovi

### 1. Testing `consortdiagram` with RCT Data

**Dataset**: `clinical_trial_consort_data.omv` (or .csv)

**Steps**:
1. Open jamovi and load `data/clinical_trial_consort_data.omv`
2. Navigate to: **OncoPathT > Study Design > CONSORT Diagram**
3. Configure options:

**Required Settings**:
- **Participant ID**: `participant_id`

**Exclusion Variables**:
- **Screening Exclusions**:
  - `age_exclusion`
  - `performance_exclusion`
  - `comorbidity_exclusion`

- **Enrollment Exclusions**:
  - `consent_exclusion`
  - `eligibility_exclusion`

- **Randomization**: `treatment_arm`

- **Post-Randomization Exclusions**: `allocation_exclusion`

- **Follow-up Exclusions**:
  - `followup_loss`
  - `followup_death`

- **Analysis Exclusions**: `analysis_exclusion`

**Display Options**:
- ✅ Show detailed exclusion reasons
- ✅ Include clinical interpretation
- ✅ CONSORT 2010 compliance check

**Expected Results**:
- Total assessed: 500
- Final analyzed: 292 (58.4% retention)
- Treatment A: ~193 randomized
- Treatment B: ~167 randomized
- Multiple exclusion details displayed

---

### 2. Testing `studydiagram` with Observational Data

**Dataset**: `observational_study_flow_data.omv`

**Steps**:
1. Load `data/observational_study_flow_data.omv`
2. Navigate to: **OncoPathT > Study Design > Study Flow Diagram**
3. Configure options:

**Settings**:
- **Participant ID**: `participant_id`
- **Exclusion Variables**:
  - Stage 1: `initial_exclusion`
  - Stage 2: `consent_status`
  - Stage 3: `followup_status`

**Expected Results**:
- Total enrolled: 350
- Final analysis: 247 (70.6% retention)
- Flow through observational cohorts

---

### 3. Testing Multi-center Scenarios

**Dataset**: `multicenter_trial_data.omv`

**Purpose**: Test complex multi-site trials with differential retention

**Settings**:
- **Participant ID**: `participant_id`
- **Screening**: `screening_failure`
- **Enrollment**: `enrollment_issue`
- **Randomization**: `arm`
- **Allocation**: `not_received`
- **Follow-up**: `followup_loss_reason`
- **Analysis**: `analysis_issue`

**Expected Results**:
- Total: 600 participants
- Final: 346 (57.7% retention)
- Site variability: 52.5% to 60.1%

---

## Comprehensive Testing Checklist

### consortdiagram Function

- [ ] **Basic Flow**
  - [ ] Loads without errors
  - [ ] Welcome message displays when no participant_id selected
  - [ ] Validation errors show for missing required variables

- [ ] **Participant Flow Table**
  - [ ] Shows all 5 stages (screening, enrollment, allocation, follow-up, analysis)
  - [ ] Remaining counts decrease correctly
  - [ ] Retention percentages calculate correctly
  - [ ] Exclusion details populate

- [ ] **Treatment Arm Summary**
  - [ ] Shows both Treatment A and Treatment B
  - [ ] Allocated/Received/Completed/Analyzed counts correct
  - [ ] Retention rates calculate per arm

- [ ] **Exclusion Breakdown**
  - [ ] Detailed reasons display when enabled
  - [ ] Counts sum correctly
  - [ ] Percentages calculate properly

- [ ] **CONSORT Diagram**
  - [ ] Renders without errors
  - [ ] Shows all flow boxes
  - [ ] Exclusion counts visible
  - [ ] Arms separated if randomization variable selected

- [ ] **Clinical Interpretation** (NEW)
  - [ ] Manuscript-ready summary generates
  - [ ] Quality warnings appear for high attrition
  - [ ] Arm-specific text includes both treatment groups

- [ ] **About Analysis Panel** (NEW)
  - [ ] Educational content displays
  - [ ] References links work

- [ ] **Caveats & Assumptions** (NEW)
  - [ ] Data quality assessment runs
  - [ ] Strengths identified (good retention, documented reasons)
  - [ ] Issues flagged (if applicable)

- [ ] **CONSORT Validation**
  - [ ] Checklist displays required elements
  - [ ] Recommended elements assessed

- [ ] **Export Information**
  - [ ] Format, dimensions, text wrap settings display

---

### studydiagram Function

- [ ] **Basic Flow**
  - [ ] Loads observational data correctly
  - [ ] Displays flow without randomization

- [ ] **Customization**
  - [ ] Custom labels apply
  - [ ] Diagram dimensions adjust
  - [ ] Text wrapping works

- [ ] **Multiple Diagram Types**
  - [ ] CONSORT style renders
  - [ ] Flowchart style renders
  - [ ] Swimmer plot style renders

---

## Data Quality Validation

### Verify Dataset Integrity

```r
# In R console:
load("data/clinical_trial_consort_data.rda")

# Check exclusion logic (participants should only be excluded once)
n_total <- nrow(clinical_trial_consort_data)
n_screening <- sum(!is.na(clinical_trial_consort_data$age_exclusion) |
                   !is.na(clinical_trial_consort_data$performance_exclusion) |
                   !is.na(clinical_trial_consort_data$comorbidity_exclusion))
n_enrollment <- sum(!is.na(clinical_trial_consort_data$consent_exclusion) |
                    !is.na(clinical_trial_consort_data$eligibility_exclusion))
n_randomized <- sum(!is.na(clinical_trial_consort_data$treatment_arm))
n_analyzed <- sum(!is.na(clinical_trial_consort_data$outcome_response))

cat("Total:", n_total, "\n")
cat("Screening excluded:", n_screening, "\n")
cat("Enrollment excluded:", n_enrollment, "\n")
cat("Randomized:", n_randomized, "\n")
cat("Analyzed:", n_analyzed, "\n")
```

**Expected**:
- Total: 500
- Screening excluded: ~100
- Enrollment excluded: ~40
- Randomized: ~360
- Analyzed: ~292

---

## Testing Edge Cases

### 1. Minimal Data
- Test with only participant_id and 1 exclusion variable
- Verify graceful handling

### 2. No Randomization
- Use observational_study_flow_data
- Confirm arm summary doesn't display

### 3. No Exclusions at a Stage
- Remove all values from one exclusion variable
- Check that stage shows "0 excluded"

### 4. High Attrition
- Multicenter data has ~42% attrition
- Verify quality warnings appear

### 5. Special Characters in Variable Names
- Test with variables containing spaces/symbols
- Confirm escapeVar utility works

---

## Performance Testing

### Checkpoints
- [ ] Checkpoints allow cancellation during long operations
- [ ] No freezing with large datasets
- [ ] Smooth rendering even with 600+ participants

---

## Visual Inspection

### Diagram Quality
- [ ] Text is readable
- [ ] Boxes align properly
- [ ] Arrows connect logically
- [ ] No overlapping text
- [ ] Percentages display correctly

### Clinical Outputs
- [ ] HTML formatting renders cleanly
- [ ] Copy-paste from manuscript summary works
- [ ] Links in references are clickable
- [ ] Warning colors appropriate (yellow for warnings, green for strengths)

---

## Reproducibility

To regenerate test data:
```r
source("data-raw/create_clinical_trial_flow_data.R")
```

Seed: `20251005` ensures reproducible results.

---

## Reporting Issues

If you find issues during testing:

1. **Note the dataset used**: clinical_trial_consort_data / observational / multicenter
2. **Record the settings**: Which variables were selected
3. **Capture the error**: Screenshot or error message
4. **Expected vs Actual**: What should have happened vs what did happen
5. **File location**: R/consortdiagram.b.R or R/studydiagram.b.R

---

## Success Criteria

✅ All datasets load without errors
✅ All stages populate with correct counts
✅ CONSORT diagram renders with proper flow
✅ Clinical interpretation generates meaningful text
✅ Quality warnings appear for high attrition datasets
✅ CONSORT 2010 compliance checklist completes
✅ Export settings display correctly
✅ No console errors or warnings during usage

---

**Last Updated**: 2025-10-05
**Datasets Version**: v1.0.0
**Functions Tested**: consortdiagram, studydiagram
