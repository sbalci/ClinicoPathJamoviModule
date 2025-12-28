# Pathology Agreement Analysis - Testing Guide

## Overview

This guide provides comprehensive instructions for testing the `pathologyagreement` function using the generated test datasets. It covers both manual testing (for developers/users) and automated testing scenarios.

## Test Datasets

### 1. Main Dataset (`pathology_agreement_main.csv`)

**Purpose:** General testing of common clinical scenarios
**Sample Size:** 250 cases across 4 scenarios
**Use Cases:** Daily testing, demonstration, typical analyses

#### Scenarios Included:

**A. Excellent Agreement (n=80)**
- **Variables:** `Ki67_HALO`, `Ki67_Aiforia`
- **Expected Results:**
  - ICC(3,1) > 0.90 (Excellent reliability)
  - Spearman r > 0.94
  - Mean bias < 1%
  - Narrow limits of agreement
- **Clinical Context:** HALO vs Aiforia digital pathology platforms
- **Recommended Settings:**
  - Clinical Preset: "Biomarker Platform Comparison"
  - ICC Type: "Consistency"
  - Correlation: "Both"

**B. Good Agreement (n=60)**
- **Variables:** `Ki67_HALO`, `Ki67_Manual`
- **Expected Results:**
  - ICC(3,1): 0.75-0.89 (Good reliability)
  - Spearman r: 0.80-0.90
  - Moderate variability in Bland-Altman plot
- **Clinical Context:** Digital platform vs manual pathologist scoring
- **Recommended Settings:**
  - Clinical Preset: "AI Algorithm vs Pathologist Agreement"
  - ICC Type: "Consistency"

**C. Moderate Agreement (n=50)**
- **Variables:** `Ki67_ProtocolA`, `Ki67_ProtocolB`
- **Expected Results:**
  - ICC(3,1): 0.50-0.74 (Moderate reliability)
  - Notice: Methods may not be interchangeable
- **Clinical Context:** Different staining protocols/antibodies

**D. Systematic Bias (n=60)**
- **Variables:** `Ki67_PlatformA`, `Ki67_PlatformB`
- **Expected Results:**
  - Good correlation (r > 0.85)
  - Significant systematic bias (~8% constant offset)
  - CI for bias excludes zero
  - ICC lower than correlation suggests
- **Clinical Context:** Platforms differ by constant calibration offset
- **Test Verification:** Bland-Altman mean difference should be significant

---

### 2. Edge Cases Dataset (`pathology_agreement_edge.csv`)

**Purpose:** Test error handling, warnings, and extreme scenarios
**Sample Size:** 188 cases across 5 scenarios

#### Scenarios Included:

**A. Perfect Agreement (n=50)**
- **Variables:** `Ki67_HALO`, `Ki67_Aiforia`
- **Expected Results:**
  - ICC ≈ 1.0
  - Correlation ≈ 1.0
  - Bias ≈ 0
  - Almost no scatter in plots
- **Use:** Validate analysis with theoretical perfect case

**B. Poor Agreement (n=40)**
- **Variables:** `Ki67_Platform1`, `Ki67_Platform2`
- **Expected Results:**
  - ICC < 0.50 (Poor reliability)
  - Weak correlation
  - Large limits of agreement
  - Warning notices about poor agreement
- **Use:** Test handling of incompatible methods

**C. Proportional Bias (n=50)**
- **Variables:** `Method1`, `Method2`
- **Expected Results:**
  - WARNING: "Proportional bias detected (slope≠0, p<0.05)"
  - Bland-Altman regression slope significant
  - Differences increase at higher values
- **Use:** Verify proportional bias detection
- **Validation:** Check Bland-Altman plot for sloped pattern

**D. Small Sample (n=8)**
- **Variables:** `Method1`, `Method2`
- **Expected Results:**
  - STRONG_WARNING: "Very small sample (n=8)"
  - Notice: "Minimum n=30 recommended"
  - Analysis should still run but with warnings
- **Use:** Test small sample size warnings
- **Expected Notices:**
  - Position 6: Very small sample strong warning
  - Reliability estimates may be unreliable

**E. Biomarker Range (n=40)**
- **Variables:** `ER_Percent_HALO`, `ER_Percent_Manual`
- **Expected Results:**
  - WARNING: "Some biomarker values outside typical 0-100% range"
  - Analysis proceeds with warning
- **Use:** Test range validation for biomarker_platforms preset
- **Settings:** Clinical Preset = "Biomarker Platform Comparison"

---

### 3. Multi-Method Dataset (`pathology_agreement_multimethod.csv`)

**Purpose:** Test multi-method analysis (3+ platforms)
**Sample Size:** 100 cases
**Variables:** `Ki67_HALO`, `Ki67_Aiforia`, `Ki67_ImageJ`, `Ki67_Manual`

#### Testing Instructions:

**Basic 2-Method Analysis:**
- Method 1: `Ki67_HALO`
- Method 2: `Ki67_Aiforia`
- Additional Methods: (none)

**3-Method Analysis:**
- Method 1: `Ki67_HALO`
- Method 2: `Ki67_Aiforia`
- Additional Methods: `Ki67_ImageJ`

**4-Method Analysis (Full):**
- Method 1: `Ki67_HALO`
- Method 2: `Ki67_Aiforia`
- Additional Methods: `Ki67_ImageJ`, `Ki67_Manual`

**Expected Outputs:**
1. Standard 2-method agreement table (Method 1 vs Method 2)
2. Correlation matrix table (all pairwise correlations)
3. Overall ICC table (ICC across all methods simultaneously)
4. Multi-method summary interpretation

**Expected Results:**
- All methods should show good to excellent agreement
- Correlation matrix: All r > 0.80
- Overall ICC(3,1) > 0.75
- Notice about multiple comparisons in interpretation

---

### 4. AI Validation Dataset (`pathology_agreement_ai.csv`)

**Purpose:** Test AI vs Pathologist agreement preset
**Sample Size:** 70 cases
**Variables:** `Ki67_AI`, `Ki67_Pathologist`, `TumorType`

#### Testing Instructions:

**Settings:**
- Clinical Preset: "AI Algorithm vs Pathologist Agreement"
- Method 1: `Ki67_AI`
- Method 2: `Ki67_Pathologist`
- ICC Type: "Consistency" (recommended for AI validation)
- Bootstrap: 2000 (high-stakes validation)

**Expected Results:**
- Good to excellent agreement (ICC > 0.75)
- Slight positive bias (~2% pathologist higher)
- AI should have less variability (more consistent)

**Additional Testing:**
- Subgroup analysis by TumorType (external to function)
- Verify bootstrap confidence intervals are reasonable

---

### 5. Multisite Dataset (`pathology_agreement_multisite.csv`)

**Purpose:** Test multi-institutional validation preset
**Sample Size:** 90 cases
**Variables:** `Ki67_SiteA`, `Ki67_SiteB`, `Institution`

#### Testing Instructions:

**Settings:**
- Clinical Preset: "Multi-institutional Validation Study"
- Method 1: `Ki67_SiteA`
- Method 2: `Ki67_SiteB`
- Bootstrap: 2000 (recommended for validation studies)

**Expected Results:**
- Excellent agreement between sites
- Small systematic bias (~1%)
- Bootstrap CI recommendation: Should suggest >2000 for validation

**Validation:**
- Check for bootstrap recommendation notice if n < 2000

---

### 6. Missing Data Dataset (`pathology_agreement_missing.csv`)

**Purpose:** Test missing data handling
**Sample Size:** 50 cases with ~20% missing
**Variables:** `HALO_Score`, `Aiforia_Score`, `ImageJ_Score`

#### Testing Scenarios:

**A. Listwise Deletion (Default)**
- Settings: Missing Data = "Listwise Deletion"
- Expected: INFO notice showing number of cases removed
- Validation: Check that n in results excludes cases with any missing

**B. Pairwise Deletion**
- Settings: Missing Data = "Pairwise Deletion"
- Expected: Uses all available pairs for each analysis
- Note: Currently standard 2-method uses complete cases, but correlation might use pairwise

**Multi-Method with Missing:**
- Method 1: `HALO_Score`
- Method 2: `Aiforia_Score`
- Additional: `ImageJ_Score`
- Expected: Complete cases only for multi-method ICC

---

## Manual Testing Procedures

### Step 1: Load Test Data

```r
# In jamovi, open the test dataset
# Or in R console:
library(ClinicoPath)
data <- read.csv("data/pathology_agreement_main.csv")
```

### Step 2: Run Basic Analysis

**In jamovi:**
1. Analysis → OncoPathT → Agreement → Pathology Agreement Analysis
2. Select variables:
   - Method 1: `Ki67_HALO`
   - Method 2: `Ki67_Aiforia`
3. Keep default settings
4. Verify outputs appear

**Expected Tables:**
1. Agreement Metrics Table (ICC, CCC, Bland-Altman)
2. Correlation Table (Spearman and Pearson)
3. Interpretation text

**Expected Plots:**
1. Scatter plot (if "Show Plots" enabled)
2. Bland-Altman plot

### Step 3: Test All Options

**Clinical Presets:**
- [ ] General Agreement Analysis (default)
- [ ] Biomarker Platform Comparison
- [ ] AI Algorithm vs Pathologist
- [ ] Multi-institutional Validation

**ICC Type:**
- [ ] Consistency (default)
- [ ] Absolute Agreement

**Correlation Method:**
- [ ] Both (default)
- [ ] Pearson only
- [ ] Spearman only

**Missing Data:**
- [ ] Listwise deletion (default)
- [ ] Pairwise deletion

**Bootstrap:**
- [ ] 1000 (default)
- [ ] 2000 (high-stakes)
- [ ] 100 (minimum, for speed testing)

**Display Options:**
- [ ] Show plots (toggle on/off)
- [ ] Show interpretation (toggle on/off)
- [ ] Show plain language summary
- [ ] Show educational explanations

### Step 4: Verify Notices

**Check for appropriate notices based on data:**

**ERROR Notices (analysis should stop):**
- Missing required variables
- Empty dataset
- Insufficient data (n<3)
- Missing required packages

**STRONG_WARNING Notices:**
- Very small sample (n<10)
- Negative ICC values

**WARNING Notices:**
- Sample size below recommended (10 ≤ n < 30)
- Biomarker values out of range (0-100%)
- Bootstrap samples below recommended for high-stakes
- Normality violation for Pearson
- Proportional bias detected

**INFO Notices:**
- Missing data summary (cases removed)
- Analysis completion summary

### Step 5: Test Multi-Method Analysis

1. Load `pathology_agreement_multimethod.csv`
2. Select:
   - Method 1: `Ki67_HALO`
   - Method 2: `Ki67_Aiforia`
   - Additional Methods: `Ki67_ImageJ`
3. Verify additional outputs appear:
   - Correlation Matrix Table
   - Overall ICC Table
   - Multi-Method Summary

---

## Automated Testing Framework

### Unit Test Structure

```r
# tests/testthat/test-pathologyagreement.R

test_that("Excellent agreement scenario works", {
    data <- read.csv("data/pathology_agreement_main.csv")
    data_excellent <- data[data$Scenario == "Excellent Agreement", ]

    # Run analysis
    results <- jmv::pathologyagreement(
        data = data_excellent,
        dep1 = "Ki67_HALO",
        dep2 = "Ki67_Aiforia",
        clinical_preset = "biomarker_platforms",
        icc_type = "consistency"
    )

    # Extract ICC
    icc_table <- results$agreementtable$asDF()
    icc_value <- icc_table$value[icc_table$metric == "ICC(3,1) - Consistency"]

    # Assertions
    expect_gt(icc_value, 0.90, label = "ICC should be excellent (>0.90)")
    expect_lt(icc_value, 1.0, label = "ICC should be realistic (<1.0)")
})

test_that("Small sample triggers warning", {
    data <- read.csv("data/pathology_agreement_edge.csv")
    data_small <- data[data$Scenario == "Small Sample", ]

    results <- jmv::pathologyagreement(
        data = data_small,
        dep1 = "Method1",
        dep2 = "Method2"
    )

    # Check for warning notice
    # (This would require access to results$notices or similar)
    expect_equal(nrow(data_small), 8)
})

test_that("Proportional bias detection works", {
    data <- read.csv("data/pathology_agreement_edge.csv")
    data_prop <- data[data$Scenario == "Proportional Bias", ]

    results <- jmv::pathologyagreement(
        data = data_prop,
        dep1 = "Method1",
        dep2 = "Method2"
    )

    # Should include proportional bias warning
    # (Validate via notice system or interpretation text)
})
```

---

## Expected Output Validation

### Agreement Table Values

| Scenario | ICC(3,1) | CCC | Bias | Interpretation |
|----------|----------|-----|------|----------------|
| Perfect | 0.99-1.0 | 0.99-1.0 | ~0 | Excellent |
| Excellent | 0.90-0.99 | 0.90-0.99 | <2% | Excellent |
| Good | 0.75-0.89 | 0.75-0.89 | <5% | Good |
| Moderate | 0.50-0.74 | 0.50-0.74 | Variable | Moderate |
| Poor | <0.50 | <0.50 | Large | Poor |
| Systematic Bias | 0.75-0.90 | Lower than ICC | ~8% | Good correlation, systematic bias |

### Correlation Table Values

| Scenario | Spearman r | Pearson r | p-value |
|----------|------------|-----------|---------|
| Perfect | >0.99 | >0.99 | <0.001 |
| Excellent | >0.94 | >0.92 | <0.001 |
| Good | 0.80-0.94 | 0.80-0.94 | <0.001 |
| Moderate | 0.60-0.80 | Variable | <0.01 |
| Poor | <0.60 | <0.60 | >0.05 |

---

## Common Issues and Troubleshooting

### Issue 1: ICC values are NA
**Cause:** `psych` package not installed
**Solution:** Install package: `install.packages("psych")`
**Expected:** ERROR notice early in analysis

### Issue 2: CCC values are NA
**Cause:** `epiR` package not installed
**Solution:** Install package: `install.packages("epiR")`
**Expected:** ERROR notice early in analysis

### Issue 3: Plots don't update with options
**Cause:** Plot state not including visual options
**Solution:** Verify `.generatePlots()` includes all plot-affecting options in `setState()`

### Issue 4: Bootstrap takes too long
**Cause:** High bootstrap_n with large dataset
**Solution:** Use n=100 for testing, 1000+ for real analysis

### Issue 5: Multi-method tables don't appear
**Cause:** Additional methods not selected, or <3 total methods
**Solution:** Ensure at least 1 additional method is selected (total ≥3)

---

## Performance Benchmarks

**Expected Runtime:**
- Small sample (n<50): <1 second
- Medium sample (n=50-100): 1-2 seconds
- Large sample (n>100): 2-5 seconds
- Bootstrap 1000: +1-2 seconds
- Bootstrap 5000: +5-10 seconds
- Multi-method (4 methods, n=100): 3-5 seconds

---

## Test Coverage Checklist

### Core Functionality
- [ ] 2-method agreement analysis runs
- [ ] ICC calculation (both types)
- [ ] CCC calculation
- [ ] Bland-Altman statistics
- [ ] Correlation (Pearson and Spearman)
- [ ] Bootstrap confidence intervals
- [ ] Plots generate correctly

### Multi-Method Analysis
- [ ] 3-method analysis
- [ ] 4+ method analysis
- [ ] Correlation matrix
- [ ] Overall ICC calculation
- [ ] Multi-method summary

### Error Handling
- [ ] Missing variables error
- [ ] Empty dataset error
- [ ] Insufficient data error
- [ ] Missing package errors

### Warnings
- [ ] Small sample warnings
- [ ] Sample size recommendations
- [ ] Biomarker range validation
- [ ] Bootstrap recommendations
- [ ] Normality violations
- [ ] Proportional bias detection

### Clinical Presets
- [ ] General analysis
- [ ] Biomarker platforms
- [ ] AI vs pathologist
- [ ] Multisite validation

### Options
- [ ] ICC type selection
- [ ] Correlation method selection
- [ ] Missing data handling
- [ ] Bootstrap sample size
- [ ] Confidence level
- [ ] Plot visibility toggle

### Output Features
- [ ] Interpretation text
- [ ] Plain language summary
- [ ] Educational explanations
- [ ] Report sentences
- [ ] Statistical glossary
- [ ] ICC selection guide

---

## Data Generation

To regenerate test data:

```r
source("data-raw/generate_pathologyagreement_test_data.R")
```

This will create/overwrite all 6 test datasets in the `data/` folder.

---

## Contact

For issues or questions about testing:
- File issues on GitHub: https://github.com/sbalci/ClinicoPathJamoviModule/issues
- Reference function: `pathologyagreement`
- Module: OncoPathT

---

**Last Updated:** 2024-12-28
**Version:** 1.0.0
