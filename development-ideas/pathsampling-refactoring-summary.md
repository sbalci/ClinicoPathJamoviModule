# Pathology Sampling Adequacy Analysis - Refactoring Summary

## Overview

The `pathsampling` module has been completely refactored to provide comprehensive analysis of pathology sampling adequacy. This document summarizes the changes, new features, and proper usage of the enhanced module.

## Critical Bug Fix

### The Problem

The original q (per-sample detection probability) estimation was fundamentally flawed:

```r
# OLD (INCORRECT) - Mixed units
totalTrials <- examinedDetected + examinedNondetected  # Number of patients
pEstimate <- nDetected / totalTrials  # Mixing patient count with probability
```

This calculated the proportion of patients with positive findings, NOT the per-sample detection probability.

### The Solution

Implemented two statistically correct estimation methods:

#### Method 1: Empirical Proportion (Preferred)

When complete positive sample counts are available:

```r
q = sum(positive_samples) / sum(total_samples)  # Among positive cases only
```

**Example:**
- Patient 1: 10 samples, 3 positive → contributes 3/10
- Patient 2: 8 samples, 1 positive → contributes 1/8
- Patient 3: 12 samples, 5 positive → contributes 5/12
- **Overall q = (3+1+5)/(10+8+12) = 9/30 = 0.30**

#### Method 2: Geometric MLE (Fallback)

When only first detection position is available:

```r
q = 1 / mean(first_detection_position)  # Among positive cases only
```

**Example:**
- Patient 1: First positive at sample 3
- Patient 2: First positive at sample 7
- Patient 3: First positive at sample 2
- **q = 1 / mean(3,7,2) = 1 / 4 = 0.25**

### Automatic Method Selection

The module automatically selects the best available method:

1. **estimationMethod = "auto"** (default):
   - Uses Empirical Proportion if `positiveCount` provided
   - Falls back to Geometric MLE if only `firstDetection` available

2. **estimationMethod = "empirical"**:
   - Forces Empirical Proportion method
   - Requires `positiveCount` variable

3. **estimationMethod = "geometric"**:
   - Forces Geometric MLE method
   - Only uses `firstDetection` variable

## New Input Variables

### Required Inputs

1. **Total Samples** (`totalSamples`)
   - Number of samples examined per case
   - Type: Numeric (integer)
   - Example: 5, 10, 15

2. **First Detection** (`firstDetection`)
   - Sample number where first positive finding occurred
   - Type: Numeric (integer)
   - Missing (NA) for negative cases
   - Example: 3 (positive at 3rd sample), NA (all negative)

### Optional Enhanced Inputs

3. **Positive Samples Count** (`positiveCount`)
   - Total number of positive samples found
   - Type: Numeric (integer)
   - Enables empirical proportion q estimation
   - Example: 3 (out of 10 samples)

4. **Positive Samples List** (`positiveSamplesList`)
   - Comma-separated list of positive sample positions
   - Type: Text/Factor
   - Enables spatial clustering analysis
   - Example: "3,5,7" or "1,2,3,4"

5. **Sample Type/Subtype** (`sampleType`)
   - Categorical grouping for stratified analysis
   - Type: Factor
   - Enables tumor biology analysis
   - Example: "Serous", "Endometrioid", "Clear Cell"

6. **Cohort Groups** (`groupBy`)
   - Additional grouping variable for comparative analysis
   - Type: Factor
   - Example: "Center A", "Center B"

## New Analyses

### 1. Empirical Cumulative Detection

**Purpose**: Directly observe detection rates at each sample threshold

**Output**: Table showing for each threshold (1, 2, 3... n samples):
- Cumulative detection rate (% of positive cases detected)
- 95% bootstrap confidence intervals
- Incremental yield (additional detection from previous threshold)

**Example Output**:
```
Samples | Detection | 95% CI        | Incremental Yield
1       | 42.1%    | [38.2-46.0]  | 42.1%
2       | 66.4%    | [62.3-70.5]  | 24.3%
3       | 79.8%    | [76.1-83.5]  | 13.4%
5       | 91.4%    | [88.7-94.1]  | 11.6%
7       | 96.2%    | [94.3-98.1]  | 4.8%
10      | 98.7%    | [97.5-99.9]  | 2.5%
```

**Interpretation**:
- With 5 samples: detect 91.4% of positive cases
- Going from 5→7 samples: gain only 4.8% additional detection

### 2. Incremental Diagnostic Yield

**Purpose**: Cost-benefit analysis of additional sampling

**Output**: Table showing marginal benefit per additional sample:
- Additional detection rate gained
- Additional cases detected per 100 patients
- Cost-benefit rating (High/Moderate/Low value)

**Example Output**:
```
From→To | Additional Detection | Cases/100 | Cost-Benefit
1→2     | 24.3%               | 24.3      | High Value
2→3     | 13.4%               | 13.4      | High Value
5→6     | 6.2%                | 6.2       | Moderate Value
9→10    | 1.3%                | 1.3       | Low Value
```

**Interpretation**:
- First 5 samples: High return on investment
- Beyond 7 samples: Diminishing returns

### 3. Sample Type Stratification

**Purpose**: Understand how tumor biology affects sampling requirements

**Output**:
- **Prevalence Table**: Positive case rate by sample type
- **Detection Probability Table**: Type-specific q estimates and detection curves

**Example Output**:

*Prevalence by Sample Type:*
```
Type          | Total | Positive | Prevalence | q estimate
Serous        | 600   | 416      | 69.3%      | 0.494
Endometrioid  | 300   | 85       | 28.3%      | 0.345
Clear Cell    | 100   | 46       | 46.0%      | 0.411
```

*Detection Probability (given tumor present):*
```
Type          | 3 samples | 5 samples | 7 samples | 10 samples
Serous        | 87.5%     | 96.8%     | 99.2%     | 99.9%
Endometrioid  | 70.2%     | 88.4%     | 95.3%     | 98.9%
Clear Cell    | 78.8%     | 93.2%     | 97.9%     | 99.7%
```

**Interpretation**:
- Serous carcinoma: High prevalence (69%) + high q (0.49) → very likely to detect
- Endometrioid: Low prevalence (28%) + moderate q (0.35) → needs more samples
- Clear Cell: Moderate prevalence (46%) + good q (0.41) → good detection

### 4. Population-Level Detection Rates

**Purpose**: Distinguish between sensitivity and overall detection

**Key Concept**:
- **Conditional probability**: P(detect | tumor present) = Sensitivity
- **Population probability**: P(detect overall) = Prevalence × Sensitivity

**Example Output**:
```
Samples | Prevalence | Sensitivity | Population Detection
3       | 47.4%      | 79.8%      | 37.8%
5       | 47.4%      | 91.4%      | 43.3%
7       | 47.4%      | 96.2%      | 45.6%
10      | 47.4%      | 98.7%      | 46.8%
```

**Interpretation**:
- Sensitivity: How well we detect tumor when present
- Population rate: What proportion of all patients test positive
- Gap between prevalence and population rate = missed cases

### 5. Spatial Clustering Analysis

**Purpose**: Identify distribution patterns of positive samples

**Method**: Clustering Index = Mean distance / Expected distance
- **< 0.7**: Clustered (tumor deposits are adjacent)
- **0.7-1.3**: Random distribution
- **> 1.3**: Dispersed (tumor deposits are spread out)

**Example Output**:
```
Pattern    | Cases | Percentage | Mean Index
Clustered  | 142   | 29.9%     | 0.48
Random     | 267   | 56.3%     | 0.98
Dispersed  | 65    | 13.7%     | 1.67
```

**Clinical Significance**:
- Clustered: Early sequential sampling may detect tumor
- Dispersed: May need more samples for reliable detection

### 6. Multifocal Detection Analysis

**Purpose**: Understand relationship between number of foci and detection timing

**Example Output**:
```
Number of Foci | Cases | Percentage | Mean First Detection
1              | 98    | 20.7%     | 5.2
2-3            | 186   | 39.2%     | 3.8
4-6            | 142   | 29.9%     | 2.4
7+             | 48    | 10.1%     | 1.3
```

**Interpretation**:
- More foci → Earlier detection
- Single focus cases → Need more samples for reliable detection

## Data Requirements by Analysis Level

### Level 1: Basic Analysis (Minimum)
**Required Variables:**
- `totalSamples`: Number of samples examined
- `firstDetection`: First positive sample position

**Available Analyses:**
- Binomial detection model
- Bootstrap sensitivity estimates
- Detection curves
- Minimum samples recommendations

**Q Estimation Method:** Geometric MLE

### Level 2: Enhanced Analysis
**Required Variables:**
- Level 1 variables +
- `positiveCount`: Total positive samples

**Additional Analyses:**
- Improved q estimation (Empirical Proportion)
- Empirical cumulative detection
- Incremental yield analysis

**Q Estimation Method:** Empirical Proportion (preferred)

### Level 3: Complete Analysis
**Required Variables:**
- Level 2 variables +
- `positiveSamplesList`: Comma-separated positive positions

**Additional Analyses:**
- Spatial clustering analysis
- Multifocal detection patterns

### Level 4: Stratified Analysis
**Required Variables:**
- Level 2 or 3 variables +
- `sampleType`: Categorical grouping

**Additional Analyses:**
- Prevalence by type
- Type-specific q estimates
- Type-specific detection curves
- Population-level detection rates

## Enhanced Simulation

Created `omentum-analysis-simulation-enhanced.R` that generates realistic test data with:

### Sample Types with Different Biology
- **Serous** (60% of cases): 70% metastasis rate, bimodal involvement (high or sparse)
- **Endometrioid** (30% of cases): 30% metastasis rate, moderate involvement
- **Clear Cell** (10% of cases): 50% metastasis rate, variable involvement

### Complete Data Tracking
- All positive sample positions recorded
- Spatial clustering metrics calculated
- Multiple tumor foci tracked

### Generated Test Datasets

1. **pathsampling_basic.csv** (Level 1)
   - patient_id, n_samples, first_pos
   - Use for basic analysis testing

2. **pathsampling_enhanced.csv** (Level 2)
   - basic + pos_count
   - Use for enhanced q estimation testing

3. **pathsampling_complete.csv** (Level 3)
   - enhanced + pos_samples_string, clustering_index
   - Use for complete analysis testing

4. **pathsampling_simulation_full.csv** (Validation)
   - complete + metastasis_present_sim (ground truth)
   - Use for validation and comparison

## Usage Examples

### Example 1: Basic Analysis (Omentum Sampling)

**Data Structure:**
```csv
patient_id,n_samples,first_pos
1,10,3
2,8,NA
3,12,1
4,10,5
```

**Analysis Setup:**
- Load data in jamovi
- OncoPathT → ClinicoPath Descriptives → Pathology Sampling Adequacy Analysis
- Select: Total Samples = n_samples, First Detection = first_pos
- Enable: Show Binomial Model, Show Bootstrap Analysis, Show Detection Curve

**Expected Results:**
- q estimate (Geometric MLE)
- Detection probability table (1-10 samples)
- Minimum samples for 95% confidence
- Bootstrap sensitivity estimates with CI
- Detection curve plot

### Example 2: Enhanced Analysis with Positive Count

**Data Structure:**
```csv
patient_id,n_samples,first_pos,pos_count
1,10,3,3
2,8,NA,0
3,12,1,5
4,10,5,2
```

**Analysis Setup:**
- Basic setup +
- Select: Positive Samples Count = pos_count
- Select: q Estimation Method = Auto (or Empirical)
- Enable: Show Empirical Cumulative Detection, Show Incremental Yield

**Expected Results:**
- Improved q estimate (Empirical Proportion)
- Empirical cumulative detection table with bootstrap CI
- Incremental yield analysis showing marginal benefit
- Comparison of empirical vs model-based detection

### Example 3: Complete Analysis with Sample Lists

**Data Structure:**
```csv
patient_id,n_samples,first_pos,pos_count,pos_samples
1,10,3,3,"3,5,7"
2,8,NA,0,NA
3,12,1,5,"1,2,3,4,5"
4,10,5,2,"5,9"
```

**Analysis Setup:**
- Enhanced setup +
- Select: Positive Samples List = pos_samples
- Enable: Show Spatial Clustering, Show Multifocal Analysis

**Expected Results:**
- All previous analyses +
- Spatial distribution patterns (clustered/random/dispersed)
- Clustering index statistics
- Multifocal detection patterns

### Example 4: Stratified Analysis by Tumor Type

**Data Structure:**
```csv
patient_id,sample_type,n_samples,first_pos,pos_count
1,Serous,10,3,3
2,Serous,8,NA,0
3,Endometrioid,12,5,2
4,Clear Cell,10,2,4
```

**Analysis Setup:**
- Enhanced or complete setup +
- Select: Sample Type = sample_type
- Enable: Show Stratified Analysis, Show Population Detection

**Expected Results:**
- Prevalence by sample type
- Type-specific q estimates
- Type-specific detection probabilities
- Population-level vs conditional detection comparison

## Clinical Recommendations Based on Results

### High-Risk Tumor Types (e.g., Serous with q ≈ 0.50)
- **Recommendation**: 5 samples achieves >95% detection
- **Rationale**: High per-sample probability means early detection likely
- **Trade-off**: Additional samples beyond 5 show minimal benefit (<2% gain)

### Moderate-Risk Tumor Types (e.g., Endometrioid with q ≈ 0.35)
- **Recommendation**: 7 samples for >95% detection
- **Rationale**: Lower per-sample probability requires more samples
- **Trade-off**: 5 samples achieves ~88%, missing ~12% of positive cases

### Clinical Decision Framework

1. **Calculate q** from your institutional data
2. **Set target confidence** (typically 95%)
3. **Determine minimum samples**: n = log(1-confidence) / log(1-q)
4. **Consider incremental yield** beyond minimum recommendation
5. **Factor in costs**: Time, resources, delay in diagnosis
6. **Adjust for tumor biology** using stratified analysis

### Stage Migration Considerations

**Concept**: More extensive sampling may upstage patients (find tumor that would have been missed)

**Analysis Strategy**:
- Compare detection rates at different thresholds
- Estimate false-negative rate with limited sampling
- Balance detection sensitivity vs. resource utilization

## Technical Notes

### Bootstrap Confidence Intervals
- Default: 10,000 iterations
- Can be adjusted via `bootstrapIterations` option
- Uses case resampling (not sample resampling)
- Accounts for between-case variability

### Spatial Clustering Algorithm
1. Sort positive sample positions
2. Calculate distances between consecutive positives
3. Compare mean distance to expected distance (uniform distribution)
4. Index < 1 indicates clustering, > 1 indicates dispersion

### Population Detection Formula
```
P(detect overall) = Prevalence × Sensitivity
                  = π × [1 - (1-q)^n]
```

Where:
- π = prevalence (proportion with positive findings)
- q = per-sample detection probability (given tumor present)
- n = number of samples examined

## Files Modified

### Core Implementation
- **R/pathsampling.b.R**: Main backend implementation (~600 lines added)
  - Fixed q estimation methods
  - Added 6 new analysis sections
  - Added 3 helper functions

### YAML Configuration
- **jamovi/pathsampling.a.yaml**: Input options
  - Added 3 new input variables
  - Added 7 new analysis controls
  - Added estimationMethod selector

- **jamovi/pathsampling.r.yaml**: Output definitions
  - Added 8 new result tables
  - Added corresponding HTML sections

- **jamovi/pathsampling.u.yaml**: UI layout (auto-generated)
  - Added controls for new inputs
  - Added checkboxes for new analyses

- **R/pathsampling.h.R**: Header file (auto-generated)

### Simulation and Documentation
- **development-ideas/omentum-analysis-simulation-enhanced.R**: New enhanced simulation
- **development-ideas/pathsampling-refactoring-summary.md**: This document

## Testing Checklist

### Basic Functionality
- [ ] Load pathsampling_basic.csv
- [ ] Run analysis with default options
- [ ] Verify q estimate is reasonable (0.1-0.9)
- [ ] Check binomial table displays correctly
- [ ] Verify detection curve renders
- [ ] Test bootstrap analysis completes

### Enhanced Functionality
- [ ] Load pathsampling_enhanced.csv
- [ ] Enable Empirical Cumulative Detection
- [ ] Verify q estimation method shows "Empirical Proportion"
- [ ] Check empirical table has bootstrap CIs
- [ ] Enable Incremental Yield analysis
- [ ] Verify cost-benefit ratings appear

### Complete Functionality
- [ ] Load pathsampling_complete.csv
- [ ] Enable Spatial Clustering
- [ ] Verify clustering patterns table displays
- [ ] Enable Multifocal Analysis
- [ ] Check foci distribution table

### Stratified Analysis
- [ ] Load pathsampling_complete.csv (has sample_type)
- [ ] Select Sample Type variable
- [ ] Enable Stratified Analysis
- [ ] Verify prevalence table by type
- [ ] Check type-specific detection tables
- [ ] Enable Population Detection
- [ ] Verify conditional vs population comparison

### Edge Cases
- [ ] Test with all negative cases (all firstDetection = NA)
- [ ] Test with all positive cases (no NA in firstDetection)
- [ ] Test with single case dataset
- [ ] Test with missing positiveCount values
- [ ] Test with invalid pos_samples_string values
- [ ] Test with single-level sampleType factor

## Future Enhancements

### Potential Additions
1. **Hypergeometric model** for finite population sampling
2. **Beta-binomial model** for overdispersed sampling
3. **Cost-effectiveness analysis** with economic modeling
4. **Sample size planning** for adequacy studies
5. **Comparison tests** between cohorts or centers
6. **Visualization enhancements**:
   - Clustering heatmaps
   - Type-specific detection curves overlay
   - Incremental yield bar charts

### Data Integration
- Export results to CSV for further analysis
- Integration with other ClinicoPath modules
- Batch processing for institutional audits

## References

### Statistical Methods
- Geometric distribution maximum likelihood estimation
- Binomial detection probability model: P(detect) = 1 - (1-q)^n
- Bootstrap confidence intervals (percentile method)
- Empirical proportion estimation

### Clinical Literature
- Maglalang & Fadare (2025): UCSD Omentum Analysis
- Goess et al. (2024): Lymph Node Analysis
- Habib et al. (2024): IPMN Lymph Node Analysis
- Ates et al. (2025): LVSI Endometrial Analysis

### Implementation References
- jamovi Module Development Guide (dev.jamovi.org)
- R6 Class System Documentation
- ClinicoPath Module Architecture

## Summary

The refactored `pathsampling` module now provides:

✅ **Correct statistical estimation** - Fixed critical q estimation bug
✅ **Three data levels** - Basic, Enhanced, Complete analysis options
✅ **Six new analyses** - Empirical cumulative, incremental yield, stratification, population detection, clustering, multifocal
✅ **Flexible q estimation** - Auto, Geometric MLE, or Empirical Proportion methods
✅ **Realistic simulation** - Enhanced simulation with tumor biology
✅ **Clinical applicability** - Generalizable to any sampling scenario (omentum, lymph nodes, margins, etc.)

The module is now ready for testing with the generated simulation datasets.
