# Waterfall Function Test Data Guide

## Overview

This guide provides comprehensive documentation for testing the waterfall function, including test datasets, automated tests, and manual testing procedures.

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

#### 1. `waterfall_percentage_basic.csv`
**Purpose**: Basic percentage change data for simple waterfall plots

**Structure**:
```csv
PatientID,Response,Treatment
PT01,-100,Drug A          # Complete Response (CR)
PT02,-85,Drug A           # Partial Response (PR)
PT03,-60,Drug A           # Partial Response (PR)
...
PT18,25,Drug B            # Progressive Disease (PD)
PT19,35,Drug B            # Progressive Disease (PD)
PT20,50,Drug B            # Progressive Disease (PD)
```

**Use Cases**:
- Basic waterfall plot visualization
- RECIST categorization validation
- Treatment group comparison
- Color scheme testing

**Variables**:
- `PatientID`: Unique patient identifier (character)
- `Response`: Percentage change from baseline (numeric, -100 to 100+)
- `Treatment`: Treatment group (factor: Drug A, Drug B)

**Key Features**:
- 20 patients total
- Covers all RECIST categories (CR, PR, SD, PD)
- Two treatment groups for comparison
- No missing values
- No extreme outliers

---

#### 2. `waterfall_raw_longitudinal.csv`
**Purpose**: Raw tumor measurements over time for spider plot generation

**Structure**:
```csv
PatientID,Time,Measurement
PT01,0,50              # Baseline measurement
PT01,2,30              # Month 2
PT01,4,25              # Month 4
PT01,6,20              # Month 6 (best response)
...
```

**Use Cases**:
- Spider plot generation
- Longitudinal response tracking
- Time-to-response analysis
- Best response calculation from raw data
- Person-time metrics calculation

**Variables**:
- `PatientID`: Unique patient identifier (character)
- `Time`: Time point in months (numeric, 0 = baseline)
- `Measurement`: Tumor size measurement (numeric, mm or cm)

**Key Features**:
- 15 patients total
- 4 time points per patient (baseline + 3 follow-ups)
- All patients have baseline (Time = 0)
- Various response patterns:
  - PT01: Continuous shrinkage (good response)
  - PT02: Initial shrinkage then stabilization
  - PT03: Stable disease
  - PT04: Progressive disease
  - PT05: Complete response (measurement → 0)

---

#### 3. `waterfall_oncology_trial.csv`
**Purpose**: Realistic clinical trial data with demographics and covariates

**Structure**:
```csv
PatientID,Response,Age,Gender,Stage,Treatment
PT001,-100,45,Male,II,Experimental
PT002,-100,45,Female,III,Experimental
...
PT050,26,40,Male,III,Experimental
```

**Use Cases**:
- Publication-ready analysis
- Clinical reporting
- Subgroup analysis
- ORR/DCR calculation with confidence intervals
- Statistical power assessment

**Variables**:
- `PatientID`: Patient ID (character)
- `Response`: Percentage change from baseline (numeric)
- `Age`: Patient age in years (numeric, 40-79)
- `Gender`: Patient sex (factor: Male, Female)
- `Stage`: Disease stage (factor: I, II, III, IV)
- `Treatment`: Treatment arm (factor: Experimental, Control)

**Key Features**:
- 50 patients (realistic trial sample size)
- Balanced treatment arms (25 each)
- Realistic age distribution (40-79 years)
- Multiple stages (I-IV)
- Response values based on typical oncology trial distributions:
  - ORR ≈ 40% (20 responders: CR+PR)
  - DCR ≈ 70% (35 patients: CR+PR+SD)

---

#### 4. `waterfall_edge_cases.csv`
**Purpose**: Test edge cases and data validation

**Structure**:
```csv
PatientID,Response
PT1,-150         # Invalid shrinkage (should be capped at -100%)
PT2,500          # Very large growth
PT3,-30          # Exact PR/SD boundary
PT4,20           # Exact SD/PD boundary
```

**Use Cases**:
- Data validation testing
- Boundary value testing
- Warning message validation
- Extreme value handling

**Key Features**:
- Invalid tumor shrinkage (<-100%)
- Extremely large growth values
- Exact RECIST boundary values
- Tests data capping and warning systems

---

#### 5. `waterfall_single_patient.csv`
**Purpose**: Single patient edge case

**Structure**:
```csv
PatientID,Response
PT001,-45
```

**Use Cases**:
- Minimum viable data testing
- Warning message for single-patient analysis
- Plot generation with n=1

---

#### 6. `waterfall_missing_baseline.csv`
**Purpose**: Missing baseline measurements (should fail validation)

**Structure**:
```csv
PatientID,Time,Measurement
PT1,2,30         # Missing time=0 (baseline)
PT1,4,25
PT2,2,45
PT2,4,40
```

**Use Cases**:
- Validation error testing
- Missing baseline detection
- Error message clarity

**Expected Behavior**: Should produce an ERROR notice about missing baseline measurements

---

#### 7. `waterfall_time_to_event.csv`
**Purpose**: Time-to-event data for person-time analysis

**Structure**:
```csv
PatientID,Time,Response,Event,EventTime
PT01,0,-60,1,4.5      # Responder, event at 4.5 months
PT02,0,-35,1,6.2
PT03,0,15,0,12.0      # Non-responder, censored at 12 months
...
```

**Use Cases**:
- Person-time metrics
- Time-to-response analysis
- Duration of response
- Survival-style analysis of response durability

**Variables**:
- `PatientID`: Patient ID
- `Time`: Measurement time point
- `Response`: Percentage change
- `Event`: Event indicator (1=event, 0=censored)
- `EventTime`: Time to event or censoring (months)

---

## Automated Test Files

### Test Suite Organization

#### 1. `tests/testthat/test-waterfall.R`
**Comprehensive main test suite**

**Test Coverage** (12 test blocks):
- ✅ Data validation (empty data, missing columns)
- ✅ RECIST categorization (CR, PR, SD, PD boundaries)
- ✅ Data processing (percentage and raw formats)
- ✅ Best response selection
- ✅ Edge cases (invalid values, single patient)
- ✅ Raw data validation (missing baseline, missing time variable)
- ✅ Plot generation (waterfall and spider plots)
- ✅ Clinical metrics (ORR, DCR calculations)
- ✅ Person-time analysis
- ✅ Function parameters (sorting, color schemes)
- ✅ Error handling
- ✅ Integration tests (full workflows)

**Running the tests**:
```r
# Run all waterfall tests
testthat::test_file("tests/testthat/test-waterfall.R")

# Run specific test
testthat::test_that("Data validation works correctly", { ... })
```

---

#### 2. `tests/testthat/test-waterfall-groups.R`
**Group-based coloring and comparison tests**

**Test Coverage** (3 test blocks):
- ✅ Default RECIST coloring
- ✅ Group-based coloring with comparison tables
- ✅ Graceful fallback when group variable missing

**Key Test Scenarios**:
```r
# Test 1: Default RECIST coloring
colorBy = "recist"  # Should categorize by CR/PR/SD/PD

# Test 2: Group-based coloring
colorBy = "group", groupVar = "TreatmentArm"  # Should compare groups

# Test 3: Missing group variable
colorBy = "group", groupVar = NULL  # Should fall back to RECIST
```

---

#### 3. `tests/testthat/test-waterfall-recist-validation.R`
**Mathematical validation of RECIST boundaries**

**Test Coverage** (6 test blocks):
- ✅ RECIST boundary values (exact boundaries, edge cases)
- ✅ ORR and DCR calculation accuracy
- ✅ Edge cases (all CR, all PD, boundary values)
- ✅ Binomial confidence intervals
- ✅ Previous bug verification (regression test)
- ✅ Complete workflow reference dataset

**RECIST Boundaries Tested**:
```r
CR:  ≤ -100%  (Complete Response)
PR:  > -100% AND ≤ -30%  (Partial Response)
SD:  > -30% AND ≤ +20%  (Stable Disease)
PD:  > +20%  (Progressive Disease)
```

**Critical Test Cases**:
- `-150%` → CR (capped at -100%)
- `-100%` → CR (exact boundary)
- `-99%` → PR
- `-30%` → PR (exact boundary)
- `-29%` → SD
- `+20%` → SD (exact boundary)
- `+21%` → PD
- `NA` → Unknown

---

#### 4. `tests/verify_waterfall.R`
**Manual verification script for development**

**Purpose**: Quick manual testing during development

**Test Scenarios**:
1. Basic percentage analysis
2. Raw data analysis with spider plot
3. Grouped analysis with custom colors
4. Missing time variable (expected failure)
5. Clinical reporting features

**Usage**:
```bash
cd /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule
Rscript tests/verify_waterfall.R
```

---

## Manual Testing Guide

### Testing in jamovi GUI

#### Quick Start
1. Open jamovi
2. Load test data: `File → Open → data/waterfall_percentage_basic.omv`
3. Navigate to: `Analyses → OncoPathT → Patient Follow-Up Plots → Treatment Response Analysis`
4. Configure variables:
   - Patient ID: `PatientID`
   - Response Value: `Response`
   - Input Type: `Percentage Changes`
   - Group Variable (optional): `Treatment`

#### Recommended Test Sequence

**Test 1: Basic Percentage Analysis**
- Data: `waterfall_percentage_basic.omv`
- Settings:
  - Input Type: Percentage Changes
  - Show Waterfall Plot: ✅
  - Show RECIST Thresholds: ✅
  - Color By: RECIST Categories
- Expected: Clear waterfall plot with 4 RECIST categories, threshold lines at -30% and +20%

**Test 2: Raw Measurements with Spider Plot**
- Data: `waterfall_raw_longitudinal.omv`
- Settings:
  - Input Type: Raw Measurements
  - Time Variable: `Time`
  - Show Waterfall Plot: ✅
  - Show Spider Plot: ✅
  - Time Unit Label: Months
- Expected: Both waterfall and spider plots, automatic percentage calculation

**Test 3: Clinical Trial Analysis**
- Data: `waterfall_oncology_trial.csv`
- Settings:
  - Group Variable: `Treatment`
  - Color By: Patient Groups
  - Color Scheme: Colorful
  - Show Confidence Intervals: ✅
  - Generate Copy-Ready Report: ✅
- Expected: Group comparison tables, ORR/DCR with CIs, publication-ready text

**Test 4: Edge Cases**
- Data: `waterfall_edge_cases.csv`
- Expected: Warning notices for:
  - Values <-100% (capped at -100%)
  - Unusually large growth values (>200%)
  - Boundary value handling

**Test 5: Error Validation**
- Data: `waterfall_missing_baseline.csv`
- Settings: Input Type = Raw Measurements
- Expected: ERROR notice about missing baseline measurements

---

### Testing in R Console

#### Basic Usage
```r
library(ClinicoPath)

# Load test data
data("waterfall_percentage_basic")

# Run basic analysis
result <- waterfall(
  data = waterfall_percentage_basic,
  patientID = "PatientID",
  responseVar = "Response",
  inputType = "percentage"
)

# Inspect results
print(result$summaryTable$asDF)
print(result$clinicalMetrics$asDF)
```

#### Advanced Testing
```r
# Test with raw longitudinal data
data("waterfall_raw_longitudinal")

result <- waterfall(
  data = waterfall_raw_longitudinal,
  patientID = "PatientID",
  responseVar = "Measurement",
  timeVar = "Time",
  inputType = "raw",
  showWaterfallPlot = TRUE,
  showSpiderPlot = TRUE
)

# Inspect person-time metrics
print(result$personTimeTable$asDF)
```

#### Group Comparison
```r
# Test group-based analysis
data("waterfall_oncology_trial")

result <- waterfall(
  data = waterfall_oncology_trial,
  patientID = "PatientID",
  responseVar = "Response",
  groupVar = "Treatment",
  inputType = "percentage",
  colorBy = "group",
  colorScheme = "colorful"
)

# Inspect group comparisons
print(result$groupComparisonTable$asDF)
```

---

## Test Coverage Summary

### Data Formats Tested
- ✅ Percentage changes (pre-calculated)
- ✅ Raw measurements (automatic calculation)
- ✅ Longitudinal data (multiple time points)
- ✅ Cross-sectional data (single time point)
- ✅ Grouped data (treatment arms, stages)

### RECIST Categories Tested
- ✅ Complete Response (CR): ≤ -100%
- ✅ Partial Response (PR): -99% to -30%
- ✅ Stable Disease (SD): -29% to +20%
- ✅ Progressive Disease (PD): > +20%
- ✅ Unknown: Missing values

### Clinical Metrics Tested
- ✅ Objective Response Rate (ORR) = (CR + PR) / N
- ✅ Disease Control Rate (DCR) = (CR + PR + SD) / N
- ✅ Exact binomial confidence intervals
- ✅ Time to response
- ✅ Duration of response
- ✅ Person-time metrics

### Edge Cases Tested
- ✅ Invalid shrinkage (<-100%)
- ✅ Extreme growth (>500%)
- ✅ Single patient (n=1)
- ✅ Missing baseline measurements
- ✅ Missing time variable
- ✅ Exact boundary values (-100%, -30%, +20%)
- ✅ All same category (all CR, all PD)

### Plot Types Tested
- ✅ Waterfall plot (bar chart)
- ✅ Spider plot (line chart)
- ✅ RECIST threshold lines
- ✅ Median response line
- ✅ Confidence interval bands
- ✅ Patient labels for outliers

### Color Schemes Tested
- ✅ jamovi (default theme)
- ✅ RECIST (Red/Blue/Green)
- ✅ Simple (Red/Green)
- ✅ Colorful (distinct group colors)
- ✅ Colorblind-safe (Okabe-Ito palette)

### Validation Tested
- ✅ Empty data detection
- ✅ Missing required columns
- ✅ Missing baseline (time=0) for raw data
- ✅ Missing time variable for raw data
- ✅ Few patients warning (n<5)
- ✅ Missing response values
- ✅ Invalid input types

---

## Adding New Test Cases

### When to Add New Tests
Add new test cases when:
1. A bug is discovered (regression test)
2. New features are added
3. Edge cases are identified in real-world usage
4. User-reported issues occur

### How to Add Test Data

#### Step 1: Create CSV File
```csv
# Save to data/waterfall_new_scenario.csv
PatientID,Response,Group
PT1,-50,A
PT2,30,B
...
```

#### Step 2: Convert to RDA and OMV
```r
# Load CSV
waterfall_new_scenario <- read.csv("data/waterfall_new_scenario.csv")

# Save as RDA (R binary)
save(waterfall_new_scenario, file = "data/waterfall_new_scenario.rda")

# Save as OMV (jamovi format)
jmvReadWrite::write_omv(waterfall_new_scenario, "data/waterfall_new_scenario.omv")
```

#### Step 3: Add to Test Suite
```r
# In tests/testthat/test-waterfall.R
test_that("New scenario works correctly", {
  data("waterfall_new_scenario")

  result <- waterfall(
    data = waterfall_new_scenario,
    patientID = "PatientID",
    responseVar = "Response",
    groupVar = "Group",
    inputType = "percentage"
  )

  # Add expectations
  expect_s3_class(result, "waterfallResults")
  expect_true(!is.null(result$summaryTable))
  # ... more expectations
})
```

#### Step 4: Document the Dataset
Add documentation to this file under "Available Test Datasets"

---

## Running All Tests

### From R Console
```r
# Run all waterfall tests
testthat::test_dir("tests/testthat", filter = "waterfall")

# Run specific test file
testthat::test_file("tests/testthat/test-waterfall.R")

# Run with detailed output
testthat::test_file("tests/testthat/test-waterfall.R", reporter = "progress")
```

### From Command Line
```bash
# Run all tests
cd /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule
Rscript -e "testthat::test_dir('tests/testthat', filter = 'waterfall')"

# Run verification script
Rscript tests/verify_waterfall.R
```

### Using devtools
```r
library(devtools)

# Run all package tests (including waterfall)
test()

# Run specific pattern
test(filter = "waterfall")
```

---

## Test Data Quality Checklist

When creating or validating test data:

- [ ] **Completeness**: All required variables present
- [ ] **Validity**: Values within expected ranges
- [ ] **Diversity**: Covers all RECIST categories
- [ ] **Edge Cases**: Includes boundary values
- [ ] **Documentation**: Purpose and structure documented
- [ ] **Formats**: Available in CSV, RDA, and OMV
- [ ] **Realistic**: Mirrors real-world clinical data
- [ ] **Sample Size**: Appropriate for intended test
- [ ] **Missing Data**: Intentional or none
- [ ] **Reproducibility**: Seeds set for random data

---

## References

### RECIST v1.1 Criteria
- Eisenhauer EA, et al. (2009). "New response evaluation criteria in solid tumours: Revised RECIST guideline (version 1.1)." European Journal of Cancer, 45(2), 228-247.

### Response Categories
- **CR (Complete Response)**: Disappearance of all target lesions (≤-100%)
- **PR (Partial Response)**: ≥30% decrease in sum of diameters (-99% to -30%)
- **SD (Stable Disease)**: Neither PR nor PD criteria met (-29% to +20%)
- **PD (Progressive Disease)**: ≥20% increase in sum of diameters (>+20%)

### Clinical Metrics
- **ORR (Objective Response Rate)**: Proportion of patients achieving CR or PR
- **DCR (Disease Control Rate)**: Proportion of patients achieving CR, PR, or SD
- **Exact Binomial CI**: Clopper-Pearson confidence intervals for proportions

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
