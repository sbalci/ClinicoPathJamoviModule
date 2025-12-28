# Swimmer Plot Function Test Data Guide

## Overview

This guide provides comprehensive documentation for testing the swimmerplot function, including test datasets, automated tests, and manual testing procedures.

## Table of Contents

1. [Test Data Files](#test-data-files)
2. [Automated Test Files](#automated-test-files)
3. [Manual Testing Guide](#manual-testing-guide)
4. [Test Coverage Summary](#test-coverage-summary)
5. [Adding New Test Cases](#adding-new-test-cases)

---

## Test Data Files

### Location
All test data files are available in multiple formats:
- **CSV files**: `/data/*.csv` - Human-readable, easy to inspect
- **RDA files**: `/data/*.rda` - R binary format for programmatic use
- **OMV files**: `/data/*.omv` - jamovi native format for GUI testing

### Available Test Datasets

#### 1. `swimmer_unified_basic.csv`
**Purpose**: Basic swimmer plot with numeric time values

**Structure**:
```csv
PatientID,StartTime,EndTime,Response,Treatment,Priority
PT001,0,12,CR,Immunotherapy,High
PT002,0,8,PR,Chemotherapy,Medium
PT003,0,15,SD,Combination,High
PT004,0,6,PD,Chemotherapy,Low
PT005,0,9,CR,Immunotherapy,High
...
```

**Use Cases**:
- Basic swimmer plot visualization
- Response category visualization
- Treatment group comparison
- Duration sorting validation

**Variables**:
- `PatientID`: Unique patient identifier (character)
- `StartTime`: Treatment start time (numeric, all start at 0)
- `EndTime`: Treatment end time (numeric, months)
- `Response`: Best response (factor: CR, PR, SD, PD)
- `Treatment`: Treatment type (factor: Immunotherapy, Chemotherapy, Combination, Targeted)
- `Priority`: Case priority (factor: High, Medium, Low)

**Key Features**:
- 10 patients total
- Covers all response categories (CR, PR, SD, PD)
- Multiple treatment groups
- Varying durations (5-15 months)
- No missing values
- All patients start at time 0 (relative timeline)

---

#### 2. `swimmer_unified_comprehensive.csv`
**Purpose**: Comprehensive data with multiple milestones and demographics

**Structure**:
```csv
PatientID,StartTime,EndTime,BestResponse,Surgery,FirstResponse,Progression,DeathLastFU,TreatmentType,AgeGroup,ECOG
PT001,0,24,CR,2,6,NA,NA,Immunotherapy,Young,0
PT002,0,18,PR,1,3,12,18,Chemotherapy,Middle,1
PT003,0,36,CR,3,9,NA,NA,Combination,Elderly,0
...
```

**Use Cases**:
- Multiple milestone visualization (up to 5 milestones)
- Clinical timeline analysis
- Demographic subgroup analysis
- Long-term follow-up visualization
- Publication-ready swimmer plots

**Variables**:
- `PatientID`: Patient ID (character)
- `StartTime`: Treatment start (numeric, 0)
- `EndTime`: End of observation (numeric, months)
- `BestResponse`: Best overall response (factor: CR, PR, SD, PD)
- `Surgery`: Time of surgery (numeric, months, may be NA)
- `FirstResponse`: Time of first response assessment (numeric, months)
- `Progression`: Time of disease progression (numeric, months, NA if none)
- `DeathLastFU`: Time of death or last follow-up (numeric, months)
- `TreatmentType`: Treatment modality (factor)
- `AgeGroup`: Age category (factor: Young, Middle, Elderly)
- `ECOG`: Performance status (numeric: 0, 1, 2)

**Key Features**:
- 20 patients with realistic clinical trajectories
- Up to 4 milestone events per patient
- Varying follow-up durations (6-51 months)
- Mix of censored and event patients
- Demographic covariates for subgroup analysis

---

#### 3. `swimmer_unified_datetime.csv`
**Purpose**: Date/time data for absolute timeline visualization

**Structure**:
```csv
PatientID,StartDate,EndDate,BestResponse,SurgeryDate,ProgressionDate
PT001,2023-01-15,2023-12-15,CR,2023-03-15,NA
PT002,2023-02-01,2023-10-01,PR,2023-03-01,2023-09-01
PT003,2023-01-20,2024-03-20,CR,2023-04-20,NA
...
```

**Use Cases**:
- Calendar-based timeline visualization
- Study enrollment timeline
- Real-world date/time handling
- Date format parsing validation
- Absolute vs relative timeline comparison

**Variables**:
- `PatientID`: Patient ID (character)
- `StartDate`: Treatment start date (character, YYYY-MM-DD format)
- `EndDate`: Treatment end date (character, YYYY-MM-DD format)
- `BestResponse`: Best overall response (factor)
- `SurgeryDate`: Date of surgery (character, may be NA)
- `ProgressionDate`: Date of progression (character, may be NA)

**Key Features**:
- 15 patients with realistic date spans
- Dates in ISO 8601 format (YYYY-MM-DD)
- Enrollment spread across multiple months
- Tests date parsing and conversion
- Tests absolute timeline display

**Date Formats to Test**:
- Default: YYYY-MM-DD (ymd)
- Also available: DD-MM-YYYY (dmy), MM-DD-YYYY (mdy)
- See `swimmer_data_date_formats.csv` for other format examples

---

#### 4. `swimmer_unified_events.csv`
**Purpose**: Event marker visualization with multiple event types

**Structure**:
```csv
PatientID,StartTime,EndTime,Response,EventType,EventTime
PT001,0,12,CR,Toxicity,3
PT001,0,12,CR,Response Assessment,6
PT001,0,12,CR,Scan,9
PT002,0,8,PR,Toxicity,2
PT002,0,8,PR,Response Assessment,4
...
```

**Use Cases**:
- Event marker visualization
- Multiple events per patient
- Event type categorization
- Timeline annotation
- Adverse event tracking

**Variables**:
- `PatientID`: Patient ID (character, may repeat for multiple events)
- `StartTime`: Treatment start (numeric, 0)
- `EndTime`: Treatment end (numeric, months)
- `Response`: Best response (factor)
- `EventType`: Type of event (factor: Toxicity, Response Assessment, Scan, Dose Modification, Hospitalization)
- `EventTime`: Time of event occurrence (numeric, months from start)

**Key Features**:
- 10 patients with multiple events each
- 5 different event types
- Events occur throughout treatment timeline
- Tests event filtering (only events ≤ EndTime shown)
- Multiple events per patient (long format data)

---

#### 5. `swimmer_unified_oncology.csv`
**Purpose**: Realistic oncology trial data for publication-quality plots

**Structure**:
```csv
PatientID,StartTime,EndTime,BestResponse,Baseline,FirstAssessment,BestResponseTime,Progression,Death,Stage,Arm,Site,Age,Gender
PT001,0,24,CR,0,8,12,NA,NA,III,Experimental,Lung,58,Male
PT002,0,16,PR,0,8,12,16,NA,IV,Control,Breast,62,Female
...
```

**Use Cases**:
- Publication-ready analysis
- Clinical trial reporting
- Subgroup analysis by stage/arm
- Overall response rate (ORR) calculation
- Disease control rate (DCR) calculation
- Group comparison (Experimental vs Control)

**Variables**:
- `PatientID`: Patient identifier
- `StartTime`: Treatment start (0)
- `EndTime`: Study completion/censoring
- `BestResponse`: Best overall response (CR, PR, SD, PD)
- `Baseline`: Baseline assessment (always 0)
- `FirstAssessment`: First response assessment time
- `BestResponseTime`: Time of best response
- `Progression`: Time of progression (NA if none)
- `Death`: Time of death (NA if censored)
- `Stage`: Disease stage (I, II, III, IV)
- `Arm`: Treatment arm (Experimental, Control)
- `Site`: Primary tumor site (Lung, Breast, Colon, etc.)
- `Age`: Patient age (years)
- `Gender`: Patient sex (Male, Female)

**Key Features**:
- 30 patients (realistic trial size)
- Balanced treatment arms (15 each)
- Multiple tumor types (6 sites)
- Full demographic data
- Realistic response distributions
- Mixed censoring and events

---

#### 6. `swimmerplot_edge_cases.csv`
**Purpose**: Edge cases and data validation testing

**Structure**:
```csv
PatientID,StartTime,EndTime,Response
PT001,0,0,CR             # Zero duration
PT002,0,-5,PR            # Negative duration (invalid)
PT003,5,10,SD            # Non-zero start
PT004,0,1000,PD          # Extremely long duration
PT005,0,5,NA             # Missing response
PT006,NA,10,CR           # Missing start
PT007,0,NA,PR            # Missing end
```

**Use Cases**:
- Data validation testing
- Error message validation
- Boundary value testing
- Missing data handling
- Invalid duration detection

**Key Features**:
- Zero-duration segments
- Negative durations (should error)
- Extremely long durations (>10 years warning)
- Missing start/end times
- Missing response values
- Non-zero start times (absolute timeline)

**Expected Behaviors**:
- Negative duration → ERROR notice
- Zero duration → WARNING notice
- Missing start/end → ERROR or exclusion with WARNING
- Extremely long duration → WARNING notice

---

#### 7. `swimmerplot_single_patient.csv`
**Purpose**: Single patient edge case

**Structure**:
```csv
PatientID,StartTime,EndTime,Response
PT001,0,12,PR
```

**Use Cases**:
- Minimum viable data testing
- Warning for n=1 analysis
- Plot generation with single patient
- Statistical analysis limitations

**Expected Behavior**: Should produce a plot but may warn about limited statistical power

---

#### 8. `swimmerplot_censoring.csv`
**Purpose**: Censoring status and ongoing treatment visualization

**Structure**:
```csv
PatientID,StartTime,EndTime,Response,CensorStatus
PT001,0,12,CR,1              # Event (completed)
PT002,0,18,PR,0              # Censored (ongoing)
PT003,0,8,SD,1               # Event
PT004,0,24,PR,0              # Censored (ongoing)
```

**Use Cases**:
- Arrow visualization for ongoing patients
- Censoring status interpretation
- Survival-style timeline visualization
- Event vs censored distinction

**Variables**:
- `CensorStatus`: 1 = event/completed, 0 = censored/ongoing
- When censored (0), an arrow is drawn at end of timeline

---

#### 9. `swimmerplot_group_comparison.csv`
**Purpose**: Group-based comparison and subgroup analysis

**Structure**:
```csv
PatientID,StartTime,EndTime,Response,TreatmentArm,ResponseStatus
PT001,0,12,CR,Experimental,1
PT002,0,8,PR,Experimental,1
PT003,0,15,SD,Experimental,0
PT004,0,6,PD,Control,0
...
```

**Use Cases**:
- Treatment arm comparison
- ORR/DCR by group
- Fisher's exact test for group differences
- Group-based coloring
- Subgroup efficacy analysis

**Variables**:
- `TreatmentArm`: Treatment group (Experimental, Control)
- `ResponseStatus`: 1 = responder (CR/PR), 0 = non-responder (SD/PD)

**Key Features**:
- Balanced groups (10 per arm)
- Different response rates between arms
- Tests Fisher's exact test implementation
- Tests group-based color palettes

---

#### 10. `swimmerplot_milestones_comprehensive.csv`
**Purpose**: Test all 5 milestone slots with realistic clinical events

**Structure**:
```csv
PatientID,StartTime,EndTime,Surgery,Treatment,Response,Progression,Death
PT001,0,24,2,4,8,NA,NA
PT002,0,18,1,3,6,15,18
PT003,0,30,3,6,12,NA,NA
...
```

**Use Cases**:
- Maximum milestone configuration testing
- Multiple marker shapes/colors
- Clinical event sequence visualization
- Treatment timeline annotation

**Milestones**:
1. Surgery: Time of surgical intervention
2. Treatment Start: Systemic therapy initiation
3. Response: First response assessment
4. Progression: Disease progression
5. Death/Last FU: Final outcome or censoring

---

## Automated Test Files

### Test Suite Organization

#### 1. `tests/testthat/test-swimmerplot.R`
**Comprehensive main test suite**

**Test Coverage** (24 test blocks):
- ✅ Minimal options (basic plot generation)
- ✅ Response variable coloring
- ✅ Date/time data handling
- ✅ Milestone markers (1-5 milestones)
- ✅ Event markers with filtering
- ✅ Plot themes (ggswim, ggswim_dark, minimal)
- ✅ Sorting options (duration, patient_id, response)
- ✅ Analysis options (person-time, response analysis)
- ✅ Export functionality (timeline, summary data)
- ✅ Invalid data handling (negative durations)
- ✅ **Person-time calculation** (overlapping interval merging)
- ✅ **Adjacent interval merging**
- ✅ **Best response selection** (oncology hierarchy: CR > PR > SD > PD)
- ✅ **ORR and DCR calculation** (from best responses)
- ✅ **Event marker filtering** (by patient end time)
- ✅ **Incidence rate calculation** (correct person-time use)
- ✅ **Case-insensitive response matching**
- ✅ **Single segment person-time**
- ✅ **Completely overlapping segments**
- ✅ **Empty response handling**
- ✅ **Multiple patients with mixed responses**

**Running the tests**:
```r
# Run all swimmerplot tests
testthat::test_file("tests/testthat/test-swimmerplot.R")

# Run specific test
testthat::test_that("person-time calculation merges overlapping intervals correctly", { ... })
```

---

#### 2. `tests/verify_swimmerplot.R`
**Manual verification script for development**

**Purpose**: Quick manual testing during development

**Test Scenarios**:
1. Basic numeric data analysis
2. Date/time data with absolute timeline
3. Complex analysis with milestones and events
4. Missing data handling

**Usage**:
```bash
cd /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule
Rscript tests/verify_swimmerplot.R
```

---

## Manual Testing Guide

### Testing in jamovi GUI

#### Quick Start
1. Open jamovi
2. Load test data: `File → Open → data/swimmer_unified_basic.omv`
3. Navigate to: `Analyses → OncoPathT → Patient Follow-Up Plots → Swimmer Plot`
4. Configure variables:
   - Patient ID: `PatientID`
   - Start Time: `StartTime`
   - End Time: `EndTime`
   - Response Variable: `Response`

#### Recommended Test Sequence

**Test 1: Basic Timeline Visualization**
- Data: `swimmer_unified_basic.omv`
- Settings:
  - Time Input Type: Raw Values (numeric)
  - Time Unit: Months
  - Time Display: Relative (all start from 0)
  - Show Legend: ✅
- Expected: Clear swimmer plot with colored lanes by response category

**Test 2: Multiple Milestones**
- Data: `swimmer_unified_comprehensive.omv`
- Settings:
  - Milestone 1: Surgery → `Surgery` variable
  - Milestone 2: First Response → `FirstResponse` variable
  - Milestone 3: Progression → `Progression` variable
  - Milestone 4: Death/Last FU → `DeathLastFU` variable
- Expected: Markers at different positions along timeline with distinct shapes

**Test 3: Date/Time with Absolute Timeline**
- Data: `swimmer_unified_datetime.omv`
- Settings:
  - Time Input Type: Date/Time
  - Date Format: YYYY-MM-DD
  - Time Display: Absolute (use actual start times)
  - Time Unit: Months
- Expected: Timeline shows actual calendar progression, not all starting at 0

**Test 4: Event Markers**
- Data: `swimmer_unified_events.omv`
- Settings:
  - Show Event Markers: ✅
  - Event Type Variable: `EventType`
  - Event Time Variable: `EventTime`
  - Marker Size: 5
- Expected: Multiple event markers along each patient timeline

**Test 5: Clinical Analysis with Groups**
- Data: `swimmer_unified_oncology.omv`
- Settings:
  - Group Variable: `Arm`
  - Include Response Analysis: ✅
  - Include Person-Time Analysis: ✅
  - Show Clinical Interpretation: ✅
- Expected: ORR/DCR tables, group comparison using Fisher's exact test

**Test 6: Color Palettes**
- Data: `swimmer_unified_basic.omv`
- Settings:
  - Color Palette: Colorblind Safe (Viridis)
  - Then test: High Contrast, Monochrome
- Expected: Different color schemes applied, colorblind-safe options work

**Test 7: Reference Lines**
- Data: `swimmer_unified_comprehensive.omv`
- Settings:
  - Reference Lines: Protocol Times (6/12/24/36)
  - Then test: Median Duration
  - Then test: Custom Time (e.g., 18 months)
- Expected: Vertical reference lines at specified time points

**Test 8: Edge Cases**
- Data: `swimmerplot_edge_cases.csv`
- Expected:
  - ERROR for negative durations
  - WARNING for zero durations
  - WARNING for extremely long follow-up
  - Proper handling of missing values

**Test 9: Single Patient**
- Data: `swimmerplot_single_patient.csv`
- Expected: Plot generates successfully, possible info notice about n=1

**Test 10: Censoring Arrows**
- Data: `swimmerplot_censoring.csv`
- Settings:
  - Censoring/Event Status Variable: `CensorStatus`
- Expected: Arrows at end of timeline for censored (ongoing) patients

---

### Testing in R Console

#### Basic Usage
```r
library(ClinicoPath)

# Load test data
data("swimmer_unified_basic")

# Run basic analysis
result <- swimmerplot(
  data = swimmer_unified_basic,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "Response"
)

# Inspect plot
print(result$swimmerPlot)
```

#### Advanced Testing: Multiple Milestones
```r
# Load comprehensive data
data("swimmer_unified_comprehensive")

result <- swimmerplot(
  data = swimmer_unified_comprehensive,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "BestResponse",
  milestone1Name = "Surgery",
  milestone1Date = "Surgery",
  milestone2Name = "First Response",
  milestone2Date = "FirstResponse",
  milestone3Name = "Progression",
  milestone3Date = "Progression",
  milestone4Name = "Death/Last FU",
  milestone4Date = "DeathLastFU",
  showInterpretation = TRUE,
  personTimeAnalysis = TRUE,
  responseAnalysis = TRUE
)

# Inspect results
print(result$clinicalInterpretation)
print(result$responseTable$asDF)
```

#### Group Comparison
```r
# Load oncology trial data
data("swimmer_unified_oncology")

result <- swimmerplot(
  data = swimmer_unified_oncology,
  patientID = "PatientID",
  startTime = "StartTime",
  endTime = "EndTime",
  responseVar = "BestResponse",
  groupVar = "Arm",
  responseAnalysis = TRUE
)

# Inspect group comparison
print(result$groupComparisonTable$asDF)
```

#### Date/Time Handling
```r
# Load datetime data
data("swimmer_unified_datetime")

result <- swimmerplot(
  data = swimmer_unified_datetime,
  patientID = "PatientID",
  startTime = "StartDate",
  endTime = "EndDate",
  timeType = "datetime",
  dateFormat = "ymd",
  timeDisplay = "absolute",
  timeUnit = "months"
)
```

---

## Test Coverage Summary

### Data Formats Tested
- ✅ Raw numeric values (days, weeks, months, years)
- ✅ Date/time formats (YYYY-MM-DD, DD-MM-YYYY, MM-DD-YYYY, etc.)
- ✅ Relative timelines (all start at 0)
- ✅ Absolute timelines (actual calendar dates)
- ✅ Multiple time units (days, weeks, months, years)

### Response Categories Tested
- ✅ Complete Response (CR)
- ✅ Partial Response (PR)
- ✅ Stable Disease (SD)
- ✅ Progressive Disease (PD)
- ✅ Not Evaluable (NE)
- ✅ Missing/Unknown responses
- ✅ Case-insensitive matching ("cr", "CR", "Complete Response")

### Milestone Events Tested
- ✅ Surgery timing
- ✅ Treatment start/modifications
- ✅ Response assessments
- ✅ Disease progression
- ✅ Death/last follow-up
- ✅ Up to 5 simultaneous milestones
- ✅ Missing milestone values (NA)

### Event Markers Tested
- ✅ Toxicity events
- ✅ Response assessments
- ✅ Imaging scans
- ✅ Dose modifications
- ✅ Hospitalizations
- ✅ Multiple events per patient
- ✅ Event time filtering (≤ patient end time)

### Clinical Metrics Tested
- ✅ Person-time calculation (correct interval merging)
- ✅ Objective Response Rate (ORR) = (CR + PR) / N
- ✅ Disease Control Rate (DCR) = (CR + PR + SD) / N
- ✅ Exact binomial confidence intervals
- ✅ Fisher's exact test for group comparison
- ✅ Best response selection (CR > PR > SD > PD hierarchy)
- ✅ Response duration metrics

### Visualization Features Tested
- ✅ Lane coloring by response category
- ✅ Lane coloring by patient group
- ✅ Multiple color palettes (default, viridis, contrast, monochrome)
- ✅ Milestone markers with distinct shapes
- ✅ Event markers along timeline
- ✅ Censoring arrows for ongoing patients
- ✅ Reference lines (median, protocol times, custom)
- ✅ Theme options (ggswim, ggswim_dark, minimal)
- ✅ Customizable lane width and marker size

### Edge Cases Tested
- ✅ Single patient (n=1)
- ✅ Zero-duration segments
- ✅ Negative durations (invalid → ERROR)
- ✅ Extremely long follow-up (>10 years → WARNING)
- ✅ Missing start/end times
- ✅ Missing response values
- ✅ Duplicate patient IDs (multiple segments)
- ✅ Overlapping time intervals (correct merging)
- ✅ Adjacent intervals (proper merging)
- ✅ Completely nested segments

### Validation Tested
- ✅ Empty data detection
- ✅ Missing required columns
- ✅ Data type mismatches (numeric vs datetime)
- ✅ Invalid date formats
- ✅ Negative durations
- ✅ Missing patient IDs
- ✅ Few patients warning (n<5)
- ✅ Response category imbalance warning

---

## Adding New Test Cases

### When to Add New Tests
Add new test cases when:
1. A bug is discovered (regression test)
2. New features are added (milestones, event types)
3. Edge cases are identified in real-world usage
4. User-reported issues occur
5. New clinical scenarios emerge

### How to Add Test Data

#### Step 1: Create CSV File
```csv
# Save to data/swimmerplot_new_scenario.csv
PatientID,StartTime,EndTime,Response,NewVariable
PT001,0,12,CR,A
PT002,0,8,PR,B
PT003,0,15,SD,A
...
```

#### Step 2: Convert to RDA and OMV
```r
# Load CSV
swimmerplot_new_scenario <- read.csv("data/swimmerplot_new_scenario.csv",
                                      stringsAsFactors = FALSE)

# Save as RDA (R binary)
save(swimmerplot_new_scenario,
     file = "data/swimmerplot_new_scenario.rda")

# Save as OMV (jamovi format)
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(swimmerplot_new_scenario,
                          "data/swimmerplot_new_scenario.omv")
}
```

#### Step 3: Add to Test Suite
```r
# In tests/testthat/test-swimmerplot.R
test_that("New scenario works correctly", {
  data("swimmerplot_new_scenario")

  result <- swimmerplot(
    data = swimmerplot_new_scenario,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    newOption = "NewVariable"
  )

  # Add expectations
  expect_true(!is.null(result))
  expect_true(!is.null(result$swimmerPlot))
  # ... more specific expectations
})
```

#### Step 4: Document the Dataset
Add documentation to this file under "Available Test Datasets"

---

## Running All Tests

### From R Console
```r
# Run all swimmerplot tests
testthat::test_dir("tests/testthat", filter = "swimmerplot")

# Run specific test file
testthat::test_file("tests/testthat/test-swimmerplot.R")

# Run with detailed output
testthat::test_file("tests/testthat/test-swimmerplot.R", reporter = "progress")
```

### From Command Line
```bash
# Run all tests
cd /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule
Rscript -e "testthat::test_dir('tests/testthat', filter = 'swimmerplot')"

# Run verification script
Rscript tests/verify_swimmerplot.R
```

### Using devtools
```r
library(devtools)

# Run all package tests (including swimmerplot)
test()

# Run specific pattern
test(filter = "swimmerplot")
```

---

## Test Data Quality Checklist

When creating or validating test data:

- [ ] **Completeness**: All required variables present (PatientID, StartTime, EndTime)
- [ ] **Validity**: Values within expected ranges (no negative durations unless testing errors)
- [ ] **Diversity**: Covers all response categories and scenarios
- [ ] **Edge Cases**: Includes boundary values and special cases
- [ ] **Documentation**: Purpose and structure documented in this guide
- [ ] **Formats**: Available in CSV, RDA, and OMV formats
- [ ] **Realistic**: Mirrors real-world clinical data patterns
- [ ] **Sample Size**: Appropriate for intended test (10-30 patients typical)
- [ ] **Missing Data**: Intentional (for testing) or none
- [ ] **Reproducibility**: Documented generation process

---

## Key Differences from Waterfall Plot

### Data Structure
- **Waterfall**: Cross-sectional (one row per patient, single measurement)
- **Swimmer**: Longitudinal (may have multiple rows per patient, timeline-based)

### Required Variables
- **Waterfall**: PatientID, Response (percentage or raw measurement)
- **Swimmer**: PatientID, StartTime, EndTime (Response optional)

### Time Handling
- **Waterfall**: Time-to-event optional, primarily focused on magnitude
- **Swimmer**: Time is central, supports relative and absolute timelines

### Milestones
- **Waterfall**: Not applicable
- **Swimmer**: Up to 5 milestone events with dates

### Events
- **Waterfall**: Not supported
- **Swimmer**: Multiple event markers along timeline

---

## References

### Clinical Trial Reporting
- CONSORT Statement for clinical trial reporting
- RECIST v1.1 for response evaluation
- Common Terminology Criteria for Adverse Events (CTCAE) v5.0

### Statistical Methods
- **ORR (Objective Response Rate)**: Proportion achieving CR or PR
- **DCR (Disease Control Rate)**: Proportion achieving CR, PR, or SD
- **Person-time**: Total unique time under observation (overlapping intervals merged)
- **Exact Binomial CI**: Clopper-Pearson confidence intervals
- **Fisher's Exact Test**: Group comparison for categorical outcomes

### Swimmer Plot Best Practices
- Sort by duration (longest first) for impact visualization
- Use consistent color schemes (colorblind-safe recommended)
- Limit to 30-50 patients for readability
- Include key milestones (treatment start, progression, death)
- Annotate censoring with arrows
- Show reference lines for protocol-defined time points

---

## Contact & Support

For questions or issues with test data:
1. Check existing issues: https://github.com/sbalci/ClinicoPathJamoviModule/issues
2. Review this guide and test files
3. Create new issue with reproducible example

---

**Last Updated**: 2025-12-28
**Version**: 1.0
**Test Suite Version**: Matches ClinicoPath v0.0.31+
