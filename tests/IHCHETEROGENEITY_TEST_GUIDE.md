# IHC Heterogeneity Test Data and Testing Guide

## Overview

This guide provides comprehensive instructions for testing the `ihcheterogeneity` function using both **manual testing** in jamovi and **automated testing** with testthat.

**Date Created:** 2025-12-28
**Function:** `ihcheterogeneity`
**Module:** ClinicoPath

---

## 📁 Test Datasets

### Available Test Datasets

All test datasets are located in: `/data/`

| Dataset | Size | Biomarker | Heterogeneity | Purpose |
|---------|------|-----------|---------------|---------|
| `ihc_heterogeneity.csv` | 100 | Ki67 | Moderate | Standard clinical scenario |
| `ihc_heterogeneity_er_low.csv` | 80 | ER H-score | Low | Excellent reproducibility |
| `ihc_heterogeneity_pdl1_high.csv` | 60 | PDL1 | High | Challenging biomarker |
| `ihc_heterogeneity_interregional.csv` | 50 | HER2 | Moderate | Inter-regional comparison (no reference) |
| `ihc_heterogeneity_small.csv` | 15 | p53 | Very high | Small sample edge cases |
| `ihc_heterogeneity_minimal.csv` | 20 | Ki67 | Moderate | Minimal viable dataset (2 regions only) |
| `ihc_heterogeneity_outliers.csv` | 40 | Ki67 | Moderate | Outlier detection |
| `ihc_heterogeneity_spatial.csv` | 80 | Ki67 | Moderate | Spatial compartment analysis |
| `ihc_heterogeneity_comprehensive.csv` | 150 | Ki67 | Moderate | Comprehensive validation |

### Dataset Characteristics

#### Ki67 Moderate Heterogeneity (Standard)
- **Variables:**
  - `case_id`: Case identifier
  - `ki67_wholesection`: Reference measurement (whole section)
  - `ki67_region1` to `ki67_region6`: Regional measurements
  - `spatial_region`: Spatial compartment (central/peripheral/invasive_front/preinvasive)
- **Expected Results:**
  - ICC: ~0.80 (good reproducibility)
  - CV: ~20% (moderate variability)
  - Correlation with reference: ~0.80-0.85

#### ER H-score Low Heterogeneity
- Mean value: ~180 (0-300 scale)
- Very high reproducibility (ICC >0.95)
- Low variability (CV <10%)
- Excellent for validation of optimal performance

#### PDL1 High Heterogeneity
- Challenging biomarker
- ICC: ~0.60 (moderate reproducibility)
- CV: >35% (high variability)
- Tests warning and quality control features

#### Inter-regional Comparison
- **No reference measurement** (no `wholesection` column)
- Tests inter-regional study design
- Bias table should be empty

---

## 🧪 Automated Testing

### Running Tests

```r
# Run all ihcheterogeneity tests
testthat::test_file("tests/testthat/test-ihcheterogeneity-comprehensive.R")

# Run specific test groups
testthat::test_file(
  "tests/testthat/test-ihcheterogeneity-comprehensive.R",
  filter = "basic functionality"
)

# Run within package development
devtools::test(filter = "ihcheterogeneity")
```

### Test Coverage

The comprehensive test suite includes **15 test groups** with **40+ individual tests**:

1. ✅ **Basic Functionality** (3 tests)
   - Module loading
   - Standard data processing
   - Minimal dataset handling

2. ✅ **Study Design** (2 tests)
   - Reference-based design
   - Inter-regional design

3. ✅ **Heterogeneity Levels** (2 tests)
   - Low heterogeneity detection
   - High heterogeneity detection

4. ✅ **Analysis Types** (4 tests)
   - Reproducibility mode
   - Bias analysis mode
   - Variability mode
   - Comprehensive mode

5. ✅ **Spatial Analysis** (2 tests)
   - Spatial region handling
   - Compartment comparison

6. ✅ **Missing Data** (2 tests)
   - Standard missing data
   - High proportion missing

7. ✅ **Edge Cases** (3 tests)
   - Small sample detection
   - Outlier detection
   - Constant values

8. ✅ **Clinical Thresholds** (2 tests)
   - CV threshold variations
   - Correlation threshold variations

9. ✅ **Output Options** (5 tests)
   - Plain language summary
   - Statistical glossary
   - Variability plots
   - Variance components
   - Power analysis

10. ✅ **Sampling Strategies** (1 test)
    - All strategy types

11. ✅ **Additional Parameters** (1 test)
    - Additional biopsies variable

12. ✅ **Comprehensive Combinations** (1 test)
    - All features enabled

13. ✅ **Input Validation** (2 tests)
    - Missing parameters
    - Invalid variable names

14. ✅ **Result Structure** (1 test)
    - Expected output components

15. ✅ **Robustness** (2 tests)
    - Extreme values
    - Random seed consistency

---

## 🖱️ Manual Testing in jamovi

### Quick Start Manual Tests

#### Test 1: Standard Reference-Based Analysis

1. **Load Data:** `ihc_heterogeneity.csv`
2. **Configure:**
   - Reference Region: `ki67_wholesection`
   - Regional Measurement 1: `ki67_region1`
   - Regional Measurement 2: `ki67_region2`
   - Regional Measurement 3: `ki67_region3`
   - Regional Measurement 4: `ki67_region4`
3. **Expected Results:**
   - Reproducibility table shows ICC ~0.80
   - Bias table shows correlations with reference
   - CV around 20%

#### Test 2: Inter-Regional Comparison (No Reference)

1. **Load Data:** `ihc_heterogeneity_interregional.csv`
2. **Configure:**
   - Leave Reference Region **EMPTY**
   - Regional Measurement 1: `her2_region1`
   - Regional Measurement 2: `her2_region2`
   - Regional Measurement 3: `her2_region3`
3. **Expected Results:**
   - Reproducibility table shows inter-regional ICC
   - Bias table empty or not shown
   - Inter-regional correlations displayed

#### Test 3: Spatial Compartment Analysis

1. **Load Data:** `ihc_heterogeneity_spatial.csv`
2. **Configure:**
   - Reference Region: `ki67_wholesection`
   - Regional Measurements: `ki67_region1`, `ki67_region2`
   - Spatial Region ID: `spatial_region`
   - Check: "Compare Spatial Compartments"
   - Check: "Compartment Comparison Tests"
3. **Expected Results:**
   - Spatial analysis table appears
   - Compartment-specific statistics shown
   - Spatial plot generated

#### Test 4: Small Sample Warning

1. **Load Data:** `ihc_heterogeneity_small.csv`
2. **Configure:**
   - Reference Region: `p53_wholesection`
   - Regional Measurements: `p53_region1`, `p53_region2`
3. **Expected Results:**
   - Warning about small sample size (n=15)
   - Analysis completes successfully
   - Interpretation includes caution note

#### Test 5: Comprehensive Analysis Mode

1. **Load Data:** `ihc_heterogeneity_comprehensive.csv`
2. **Configure:**
   - All regional measurements
   - Spatial Region ID: `spatial_region`
   - Analysis Focus: "Comprehensive Analysis"
   - Check ALL output options:
     - Variance Component Analysis
     - Power Analysis
     - Show Variability Plots
     - Show Plain-Language Summary
     - Show Statistical Glossary
3. **Expected Results:**
   - All tables populated
   - All plots generated
   - Summary and glossary displayed

---

## 📊 Test Scenarios

See `data/ihc_heterogeneity_test_scenarios.csv` for 20 detailed test scenarios covering:

- Different analysis types
- Threshold sensitivity
- Clinical interpretations
- Edge cases
- Feature combinations

### Key Scenarios to Test Manually

| ID | Scenario | Dataset | Key Settings |
|----|----------|---------|--------------|
| 1 | Basic reference-based | `ihc_heterogeneity.csv` | Default settings |
| 2 | Inter-regional | `ihc_heterogeneity_interregional.csv` | No reference |
| 3 | High heterogeneity | `ihc_heterogeneity_pdl1_high.csv` | CV threshold 20% |
| 4 | Low heterogeneity | `ihc_heterogeneity_er_low.csv` | Correlation 0.90 |
| 7 | Outlier detection | `ihc_heterogeneity_outliers.csv` | Look for warnings |
| 8 | Spatial comparison | `ihc_heterogeneity_spatial.csv` | Enable compartment tests |
| 15 | Comprehensive | `ihc_heterogeneity_comprehensive.csv` | All features ON |

---

## ✅ Validation Checklist

### Functionality Validation

- [ ] Function loads without errors
- [ ] Reference-based analysis works
- [ ] Inter-regional analysis works
- [ ] Spatial analysis functional
- [ ] All analysis types selectable
- [ ] Plots generate correctly
- [ ] Tables populate properly

### Statistical Validation

- [ ] ICC values in valid range (-1 to 1)
- [ ] CV values reasonable (<100% typically)
- [ ] Correlations in expected range
- [ ] Missing data handled appropriately
- [ ] Outliers detected correctly

### User Experience Validation

- [ ] Welcome screen informative
- [ ] Variable labels clear
- [ ] Error messages helpful
- [ ] Plain language summary readable
- [ ] Glossary definitions accurate
- [ ] Warnings appropriate
- [ ] Recommendations actionable

---

## 🔧 Troubleshooting

### Common Issues

**Issue:** "No data available" error
- **Solution:** Check that variable names match exactly (case-sensitive)
- **Solution:** Ensure at least `biopsy1` (Regional Measurement 1) is specified

**Issue:** Empty tables
- **Solution:** Verify sufficient non-missing data
- **Solution:** Check that variables contain numeric continuous data

**Issue:** No spatial analysis table
- **Solution:** Ensure `spatial_id` variable is specified
- **Solution:** Verify spatial_id is a factor/categorical variable

**Issue:** High CV or warnings
- **Solution:** This may be expected for heterogeneous biomarkers (like PDL1)
- **Solution:** Review data quality and measurement protocol

---

## 📝 Reporting Issues

When reporting issues, include:

1. **Dataset used:** Which test dataset
2. **Settings:** All option values
3. **Expected vs Actual:** What you expected vs what happened
4. **Screenshots:** Of results or error messages
5. **jamovi version:** Help > About jamovi
6. **ClinicoPath version:** From module info

---

## 🚀 Next Steps

After testing:

1. Review all test results
2. Document any issues found
3. Verify statistical outputs match expectations
4. Check clinical interpretations for accuracy
5. Validate against known benchmarks or published studies

---

## 📚 References

### Statistical Concepts

- **ICC (Intraclass Correlation Coefficient):**
  - >0.90: Excellent reliability
  - 0.75-0.90: Good reliability
  - 0.50-0.75: Moderate reliability
  - <0.50: Poor reliability

- **CV (Coefficient of Variation):**
  - <10%: Excellent reproducibility
  - 10-20%: Good reproducibility
  - 20-30%: Moderate variability
  - >30%: High variability

- **Correlation:**
  - >0.80: Strong relationship
  - 0.60-0.80: Moderate relationship
  - <0.60: Weak relationship

### Clinical Applications

- Ki67 proliferation index (0-100%)
- ER/PR H-scores (0-300)
- PDL1 tumor proportion score (0-100%)
- HER2 quantitative assessment

---

**Last Updated:** 2025-12-28
**Maintainer:** ClinicoPath Development Team
