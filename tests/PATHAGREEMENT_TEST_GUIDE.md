# Pathology Interrater Reliability (pathagreement) Test Guide

## Overview

This guide explains the test data and testing procedures for the `pathagreement` function in ClinicoPath jamovi module.

## Quick Start

```r
# Generate all test datasets
Rscript tests/generate_pathagreement_test_data.R

# Run automated tests
testthat::test_file('tests/testthat/test-pathagreement.R')

# Manual testing in jamovi
# Open any data/pathagreement_*.omv file
```

---

## Test Datasets

### Dataset 1: Two-Rater Agreement (Cohen's Kappa)
**File:** `pathagreement_two_raters.{csv,rda,omv}`

- **Purpose:** Basic Cohen's kappa calculation
- **Structure:**
  - 50 cases
  - 2 pathologists (A & B)
  - 3 ordinal categories: Benign → Borderline → Malignant
  - ~70% observed agreement

**Usage Example:**
```r
data("pathagreement_two_raters", package = "ClinicoPath")
pathagreement(
  data = pathagreement_two_raters,
  vars = c("Pathologist_A", "Pathologist_B")
)
```

**jamovi Test:**
1. Open `pathagreement_two_raters.omv`
2. Analysis → OncoPathT → Agreement → Pathology Interrater Reliability
3. Select both Pathologist_A and Pathologist_B as Raters
4. Check: Agreement should be ~0.5-0.7 (moderate)

---

### Dataset 2: Multi-Rater Agreement (Fleiss' Kappa)
**File:** `pathagreement_multi_raters.{csv,rda,omv}`

- **Purpose:** Fleiss' kappa and Krippendorff's alpha
- **Structure:**
  - 60 cases
  - 5 raters
  - 3 ordinal categories
  - ~65% agreement

**Usage Example:**
```r
data("pathagreement_multi_raters", package = "ClinicoPath")
pathagreement(
  data = pathagreement_multi_raters,
  vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4", "Rater_5"),
  multiraterMethod = "fleiss",
  fleissCI = TRUE
)
```

**What to Test:**
- ✅ Fleiss' kappa calculation
- ✅ Confidence intervals
- ✅ Krippendorff's alpha (nominal and ordinal)
- ✅ Pairwise comparison tables
- ✅ Consensus analysis

---

### Dataset 3: Breast Pathology with Reference Standard
**File:** `pathagreement_breast.{csv,rda,omv}`

- **Purpose:** Agreement vs reference standard
- **Structure:**
  - 100 breast biopsy cases
  - 6 pathologists + reference standard
  - 4 ordinal categories: Benign → Atypical → DCIS → Invasive
  - Varying expertise levels (85% to 60% agreement with reference)

**Usage Example:**
```r
data("pathagreement_breast", package = "ClinicoPath")
raters <- paste0("Path_", 1:6)
pathagreement(
  data = pathagreement_breast,
  vars = raters,
  pathologyContext = TRUE
)
```

**What to Test:**
- ✅ Sensitivity/specificity vs reference
- ✅ Case difficulty stratification
- ✅ Pathologist performance metrics

---

### Dataset 4: Diagnostic Style Clustering (Usubutun Method)
**File:** `pathagreement_clustering.{csv,rda,omv}`

- **Purpose:** Identify diagnostic style groups
- **Structure:**
  - 80 endometrial cases
  - 12 pathologists (A-L)
  - 3 categories: Benign → EIN → Adenocarcinoma
  - **Three hidden style groups:**
    - Conservative (Raters A-D): Bias toward lower grades
    - Moderate (Raters E-H): Balanced, accurate
    - Aggressive (Raters I-L): Bias toward higher grades

**Companion File:** `pathagreement_clustering_rater_info.csv`
- Contains rater characteristics (experience, institution, volume, etc.)

**Usage Example:**
```r
data("pathagreement_clustering", package = "ClinicoPath")
pathagreement(
  data = pathagreement_clustering,
  vars = LETTERS[1:12],
  performClustering = TRUE,
  clusteringMethod = "ward",
  nStyleGroups = 3,
  showClusteringHeatmap = TRUE,
  identifyDiscordant = TRUE
)
```

**What to Test:**
- ✅ Ward's, complete, and average linkage clustering
- ✅ Automatic optimal group selection
- ✅ Clustering heatmap visualization
- ✅ Discordant case identification
- ✅ Association with rater characteristics

**Expected Result:**
- Should identify 3 groups corresponding to conservative/moderate/aggressive styles
- Conservative group should show bias toward "Benign"
- Aggressive group should show bias toward "Adenocarcinoma"

---

### Dataset 5: Melanoma Classification (Nominal Categories)
**File:** `pathagreement_melanoma.{csv,rda,omv}`

- **Purpose:** Nominal (unordered) categories
- **Structure:**
  - 70 pigmented lesions
  - 4 dermatopathologists
  - 5 **nominal** categories (no inherent order):
    - Benign Nevus
    - Dysplastic Nevus
    - Melanoma in situ
    - Invasive Melanoma
    - Spitzoid Lesion
  - ~60% agreement (difficult diagnoses)

**Usage Example:**
```r
data("pathagreement_melanoma", package = "ClinicoPath")
pathagreement(
  data = pathagreement_melanoma,
  vars = c("Dermpath_1", "Dermpath_2", "Dermpath_3", "Dermpath_4"),
  wght = "unweighted",  # Nominal data
  kripp = TRUE,
  krippMethod = "nominal"
)
```

**What to Test:**
- ✅ Unweighted kappa (nominal appropriate)
- ✅ Krippendorff's alpha nominal method
- ⚠️ Weighted kappa should NOT be used (or warn user)

---

### Dataset 6: Perfect Agreement (Edge Case)
**File:** `pathagreement_perfect.{csv,rda,omv}`

- **Purpose:** Test kappa = 1.0 scenario
- **Structure:**
  - 20 cases
  - 3 raters
  - 100% agreement
  - 2 categories: Benign, Malignant

**Usage Example:**
```r
data("pathagreement_perfect", package = "ClinicoPath")
pathagreement(
  data = pathagreement_perfect,
  vars = c("Rater_1", "Rater_2", "Rater_3")
)
```

**Expected Result:**
- Kappa = 1.0
- CI should be narrow
- No warnings or errors

---

### Dataset 7: Complete Disagreement (Edge Case)
**File:** `pathagreement_disagreement.{csv,rda,omv}`

- **Purpose:** Test kappa ≤ 0 scenario
- **Structure:**
  - 15 cases
  - 3 raters
  - Maximal disagreement (each rater chooses different category every time)
  - 3 categories

**Usage Example:**
```r
data("pathagreement_disagreement", package = "ClinicoPath")
pathagreement(
  data = pathagreement_disagreement,
  vars = c("Rater_1", "Rater_2", "Rater_3")
)
```

**Expected Result:**
- Kappa near 0 or negative
- Wide CI
- Function should handle gracefully (not crash)

---

### Dataset 8: Missing Data Patterns
**File:** `pathagreement_missing.{csv,rda,omv}`

- **Purpose:** Test missing value handling
- **Structure:**
  - 40 cases
  - 4 raters
  - ~10% missing values randomly distributed

**Usage Example:**
```r
data("pathagreement_missing", package = "ClinicoPath")
pathagreement(
  data = pathagreement_missing,
  vars = c("Rater_1", "Rater_2", "Rater_3", "Rater_4")
)
```

**What to Test:**
- ✅ Pairwise deletion (default)
- ✅ Listwise deletion option
- ✅ Warning messages about missing data
- ✅ Effective sample size reporting

---

### Dataset 9: Single Case (Minimum Data)
**File:** `pathagreement_single.{csv,rda,omv}`

- **Purpose:** Test minimum viable input
- **Structure:**
  - 1 case
  - 2 raters
  - Disagree

**Usage Example:**
```r
data("pathagreement_single", package = "ClinicoPath")
pathagreement(
  data = pathagreement_single,
  vars = c("Rater_1", "Rater_2")
)
```

**Expected Result:**
- May produce error OR warning
- If error, should be informative ("Insufficient cases")
- Should not crash jamovi

---

### Dataset 10: Comprehensive Test Dataset
**File:** `pathagreement_comprehensive.{csv,rda,omv}`

- **Purpose:** Test all features simultaneously
- **Structure:**
  - 120 cases
  - 8 pathologists + reference
  - Multiple metadata variables:
    - Subspecialty (Breast, GI, GU, Derm, Lung, Heme)
    - Case difficulty (Easy, Moderate, Difficult)
    - Image quality (Excellent, Good, Fair, Poor)
  - Discordant case flagging

**Usage Example:**
```r
data("pathagreement_comprehensive", package = "ClinicoPath")
raters <- paste0("Pathologist_", LETTERS[1:8])
pathagreement(
  data = pathagreement_comprehensive,
  vars = raters,
  multiraterMethod = "fleiss",
  fleissCI = TRUE,
  heatmap = TRUE,
  heatmapDetails = TRUE,
  sft = TRUE,
  pairwiseAnalysis = TRUE,
  categoryAnalysis = TRUE,
  showClinicalSummary = TRUE
)
```

**What to Test:**
- ✅ All kappa methods
- ✅ All visualization options
- ✅ All advanced analyses
- ✅ Performance with larger dataset

---

## Feature Testing Matrix

| Feature | Dataset to Use | Test Type |
|---------|---------------|-----------|
| Cohen's kappa | two_raters, perfect, disagreement | Basic |
| Fleiss' kappa | multi_raters, comprehensive | Multi-rater |
| Krippendorff's α (nominal) | melanoma | Nominal |
| Krippendorff's α (ordinal) | multi_raters, breast | Ordinal |
| Weighted kappa (squared) | two_raters, breast | Ordinal |
| Weighted kappa (linear) | two_raters, breast | Ordinal |
| Frequency tables | two_raters, multi_raters | Visualization |
| Agreement heatmap | multi_raters, comprehensive | Visualization |
| Heatmap themes | multi_raters | Visualization |
| Consensus analysis | multi_raters, comprehensive | Advanced |
| Majority voting | multi_raters | Consensus |
| Super majority | multi_raters | Consensus |
| Unanimous | multi_raters | Consensus |
| Diagnostic style clustering | clustering | Advanced |
| Ward's linkage | clustering | Clustering |
| Discordant cases | clustering, comprehensive | Clustering |
| Auto group selection | clustering | Clustering |
| ICC calculation | multi_raters | Advanced |
| Pairwise analysis | multi_raters, comprehensive | Advanced |
| Category-specific agreement | multi_raters, breast | Advanced |
| Gwet's AC | two_raters | Advanced |
| PABAK | two_raters | Advanced |
| Reference standard | breast | Validation |
| Missing data | missing | Edge case |
| Perfect agreement | perfect | Edge case |
| No agreement | disagreement | Edge case |
| Single case | single | Edge case |

---

## Manual Testing in jamovi

### Basic Test (5 minutes)

1. **Open dataset:** `pathagreement_two_raters.omv`
2. **Navigate:** Analysis → OncoPathT → Agreement → Pathology Interrater Reliability
3. **Select variables:** Both pathologists as raters
4. **Check output:**
   - Cohen's kappa value
   - Confidence interval
   - p-value
5. **Enable visualizations:**
   - ☑ Frequency Tables
   - ☑ Agreement Heatmap
6. **Verify plots appear**

### Intermediate Test (15 minutes)

1. **Open dataset:** `pathagreement_multi_raters.omv`
2. **Select all 5 raters**
3. **Configure:**
   - Multi-rater Method: Fleiss' Kappa
   - ☑ Fleiss Kappa Confidence Intervals
   - ☑ Krippendorff's Alpha
   - Data Type: Ordinal
4. **Consensus Analysis:**
   - ☑ Consensus Analysis
   - Method: Majority Rule
   - ☑ Show Consensus Results
5. **Verify outputs:**
   - Fleiss' kappa table
   - Krippendorff's alpha
   - Consensus summary
   - Consensus table with individual cases

### Advanced Test (30 minutes)

1. **Open dataset:** `pathagreement_clustering.omv`
2. **Select all 12 raters (A-L)**
3. **Basic Agreement:**
   - Multi-rater Method: Fleiss' Kappa
   - ☑ Frequency Tables
   - ☑ Pairwise Rater Analysis
4. **Diagnostic Style Clustering:**
   - ☑ Perform Rater Clustering Analysis
   - Clustering Method: Ward's method
   - Number of Style Groups: 3
   - ☑ Show Clustering Heatmap
   - ☑ Identify Discordant Cases
   - Threshold: 0.5
5. **Verify outputs:**
   - Should identify 3 groups
   - Heatmap should show clear patterns
   - Discordant cases should be flagged
6. **Interpretation:**
   - Check if raters A-D cluster together (conservative)
   - Check if raters E-H cluster together (moderate)
   - Check if raters I-L cluster together (aggressive)

---

## Automated Testing

Run all automated tests:

```r
# Run full test suite
testthat::test_file('tests/testthat/test-pathagreement.R')

# Run specific test
testthat::test_file(
  'tests/testthat/test-pathagreement.R',
  filter = "Cohen's kappa"
)
```

**Expected Results:**
- All tests should pass
- No warnings or errors
- Coverage of all major features

---

## Common Issues and Solutions

### Issue: "Data not found" error

**Solution:**
```r
# Ensure data is loaded in package
devtools::load_all()  # If testing during development

# OR install package first
devtools::install()
library(ClinicoPath)
```

### Issue: kappa = NA or Inf

**Cause:** Perfect agreement or single category

**Expected:** Should handle gracefully, not crash

### Issue: Clustering doesn't find expected groups

**Diagnosis:** Check dendrograms and silhouette plots

**Note:** Clustering is exploratory; exact groups may vary slightly

### Issue: Missing data warnings

**Expected:** Should warn but complete analysis

**Check:** Effective sample sizes should be reported

---

## Performance Benchmarks

| Dataset | n_cases | n_raters | Expected Time |
|---------|---------|----------|---------------|
| two_raters | 50 | 2 | < 1 sec |
| multi_raters | 60 | 5 | < 2 sec |
| breast | 100 | 6 | < 3 sec |
| clustering | 80 | 12 | < 5 sec |
| melanoma | 70 | 4 | < 2 sec |
| comprehensive | 120 | 8 | < 4 sec |

**Note:** With clustering + bootstrap: add 5-10 seconds

---

## Validation Criteria

### Mathematical Accuracy

- ✅ Cohen's kappa matches `irr::kappa2()`
- ✅ Fleiss' kappa matches `irr::kappam.fleiss()`
- ✅ Krippendorff's α matches `irr::kripp.alpha()`

### Clinical Interpretation

- ✅ Kappa < 0.20: Poor agreement
- ✅ Kappa 0.21-0.40: Fair agreement
- ✅ Kappa 0.41-0.60: Moderate agreement
- ✅ Kappa 0.61-0.80: Substantial agreement
- ✅ Kappa 0.81-1.00: Almost perfect agreement

### Edge Cases

- ✅ Handles perfect agreement (κ = 1.0)
- ✅ Handles no agreement (κ ≤ 0)
- ✅ Handles missing data
- ✅ Handles small samples (n < 10)
- ✅ Handles large samples (n > 100)
- ✅ Handles many raters (> 10)

---

## Regenerating Test Data

If you need to regenerate test data (e.g., after changing data structure):

```bash
# Regenerate all datasets
Rscript tests/generate_pathagreement_test_data.R

# Verify files were created
ls -lh data/pathagreement_*.{csv,rda,omv}

# Check data structure
R -e "load('data/pathagreement_two_raters.rda'); str(pathagreement_two_raters)"
```

**Data Generation Features:**
- ✅ Reproducible (set.seed(2025))
- ✅ Realistic clinical scenarios
- ✅ Controlled agreement levels
- ✅ All formats (CSV, RDA, OMV)

---

## Contributing

When adding new features to `pathagreement`:

1. **Add test data** in `generate_pathagreement_test_data.R`
2. **Add test cases** in `tests/testthat/test-pathagreement.R`
3. **Update this guide** with new test scenarios
4. **Run full test suite** to ensure no regressions

---

## References

- Landis JR, Koch GG. The measurement of observer agreement for categorical data. Biometrics. 1977;33(1):159-174.
- Fleiss JL. Measuring nominal scale agreement among many raters. Psychological Bulletin. 1971;76(5):378-382.
- Krippendorff K. Content Analysis: An Introduction to Its Methodology. 2nd ed. Sage; 2004.
- Usubutun A, Mutter GL, Saglam A, et al. Reproducibility of endometrial intraepithelial neoplasia diagnosis is good, but influenced by the diagnostic style of pathologists. Modern Pathology. 2012;25(6):877-884.
- Cohen J. A coefficient of agreement for nominal scales. Educational and Psychological Measurement. 1960;20(1):37-46.

---

## Contact

For issues or questions about testing:
- GitHub: https://github.com/sbalci/ClinicoPathJamoviModule/issues
- Documentation: https://www.serdarbalci.com/ClinicoPathJamoviModule/

---

**Last Updated:** 2025-12-28
**Test Data Version:** 1.0
**Module Version:** 0.0.31+
