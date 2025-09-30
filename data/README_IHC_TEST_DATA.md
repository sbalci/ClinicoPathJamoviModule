# IHC Breast Cancer Test Dataset

## 📋 Overview

Comprehensive, realistic breast cancer immunohistochemistry (IHC) dataset designed for testing the `ihccluster` function in ClinicoPath jamovi module.

- **Cases**: 155 (150 realistic + 5 edge test cases)
- **Subtypes**: 4 molecular subtypes (Luminal A, Luminal B, HER2+, Triple Negative)
- **Markers**: 5 categorical + 3 continuous IHC markers
- **Clinical**: Age, grade, stage, size, lymph nodes
- **Survival**: Overall survival time and events
- **Missing Data**: ~10% realistic missingness

---

## 📂 Files

| File | Description |
|------|-------------|
| `ihc_breast_cancer.csv` | Main dataset (CSV format for jamovi) |
| `ihc_breast_cancer.rds` | R binary format (faster loading) |
| `ihc_breast_cancer_codebook.txt` | Variable definitions |
| `../data-raw/generate_ihc_test_data.R` | Data generation script |
| `../data-raw/test_ihccluster_features.R` | Comprehensive test scenarios |

---

## 📊 Dataset Structure

### Identifiers

| Variable | Type | Description |
|----------|------|-------------|
| `PatientID` | Character | Unique identifier (BC-0001 to BC-0155) |
| `TrueSubtype` | Factor | Ground truth molecular subtype for validation |

### Categorical IHC Markers

| Variable | Levels | Description |
|----------|--------|-------------|
| `ER_Status` | Negative, Positive | Estrogen Receptor |
| `PR_Status` | Negative, Positive | Progesterone Receptor |
| `HER2_IHC` | 0, 1+, 2+, 3+ | HER2 immunohistochemistry |
| `CK5_6` | Negative, Positive | Cytokeratin 5/6 (basal marker) |
| `EGFR` | Negative, Positive | Epidermal Growth Factor Receptor |

### Continuous IHC Markers

| Variable | Range | Description |
|----------|-------|-------------|
| `Ki67_Percent` | 0-100% | Proliferation index |
| `AR_Hscore` | 0-300 | Androgen Receptor H-score |
| `p53_Percent` | 0-100% | p53 nuclear staining percentage |

### Clinical Variables

| Variable | Type | Description |
|----------|------|-------------|
| `Age_Years` | Integer | Age at diagnosis (30-90 years) |
| `Tumor_Grade` | Factor (G1/G2/G3) | Histologic grade |
| `Tumor_Stage` | Factor (I/II/III/IV) | Clinical stage |
| `Tumor_Size_cm` | Numeric | Tumor size in centimeters |
| `Lymph_Node_Status` | Factor | Lymph node involvement |

### Survival Data

| Variable | Type | Description |
|----------|------|-------------|
| `OS_Months` | Numeric | Overall survival time in months |
| `OS_Event` | Integer | Death event (0=censored, 1=death) |

---

## 🧬 Molecular Subtypes (Ground Truth)

| Subtype | N | IHC Profile | Expected Cluster |
|---------|---|-------------|------------------|
| **Luminal A** | 60 (38.7%) | ER+/PR+/HER2-/low Ki67 | Should cluster together |
| **Luminal B** | 40 (25.8%) | ER+/PR+/high Ki67 or HER2+ | May split into 2 groups |
| **HER2 Positive** | 25 (16.1%) | ER-/PR-/HER2 2+/3+ | Should cluster together |
| **Triple Negative** | 25 (16.1%) | ER-/PR-/HER2-/high CK5/6 | Should cluster together |
| **Test Cases** | 5 (3.2%) | Extreme values | Edge case testing |

---

## 📈 Data Characteristics

### Missing Data Pattern

| Variable | Missing | % |
|----------|---------|---|
| AR_Hscore | 18 | 11.6% |
| p53_Percent | 15 | 9.7% |
| Ki67_Percent | 12 | 7.7% |
| EGFR | 11 | 7.1% |
| CK5_6 | 8 | 5.2% |
| Tumor_Size_cm | 9 | 5.8% |

### Survival Summary

- **Median OS**: 46.1 months
- **Events**: 78 (50.3%)
- **Censored**: 77 (49.7%)

### Expected Clustering Results

With appropriate settings (e.g., PAM with ER, PR, HER2, Ki67), the analysis should identify **3-4 clusters** roughly corresponding to:

1. **Luminal A-like**: ER+/PR+/HER2-/low Ki67 (~60 cases)
2. **Luminal B-like**: ER+/PR+/high Ki67 or HER2+ (~40-45 cases)
3. **HER2-enriched**: ER-/PR-/HER2 2+/3+ (~25 cases)
4. **Triple Negative**: ER-/PR-/HER2-/CK5/6+ (~25 cases)

Some mixing between clusters is expected and realistic.

---

## 🧪 Quick Start Guide

### 1. Load Data in jamovi

```
File → Open → data/ihc_breast_cancer.csv
```

### 2. Run Basic Clustering

**Menu**: Analyses → OncoPathT → IHC Clustering Analysis

**Settings**:
- Categorical IHC Markers: `ER_Status`, `PR_Status`, `HER2_IHC`
- Continuous IHC Markers: `Ki67_Percent`
- Clustering Method: PAM (k-medoids)
- Number of Clusters: Auto-select optimal k
- Show Silhouette Plot: ✓
- Show Heatmap: ✓

**Expected Result**: 3-4 clusters identified with good separation

### 3. Test New Features

**Natural Language Summary**:
```
Show Plain-Language Summary: ✓
Language: English
```

**Contextual Warnings**:
```
Show Analysis Warnings: ✓
Number of Clusters: 8  (will trigger overfitting warning)
```

**Statistical Glossary**:
```
Show Statistical Glossary: ✓
Language: English or Turkish
```

---

## 📝 Comprehensive Test Scenarios

### Test 1: Basic Clustering
```
Categorical: ER_Status, PR_Status, HER2_IHC
Continuous: Ki67_Percent
Method: PAM
Auto-select k: Yes
Expected: 3-4 clusters
```

### Test 2: Hierarchical with Dendrogram
```
Categorical: ER_Status, PR_Status, HER2_IHC, CK5_6
Continuous: Ki67_Percent, AR_Hscore
Method: Hierarchical
Fixed k: 4
Show Dendrogram: Yes
Expected: Tree structure showing relationships
```

### Test 3: All Markers (Dimension Reduction)
```
Categorical: All 5 markers
Continuous: All 3 markers
Method: MCA/PCA + k-means
Show PCA Plot: Yes
Expected: 2D plot with cluster overlay
```

### Test 4: Missing Data Strategies
```
Test 4a - Complete cases:
  Handle Missing: Complete cases only
  Expected: ~130 cases analyzed

Test 4b - Pairwise:
  Handle Missing: Pairwise distances
  Expected: All 155 cases analyzed
```

### Test 5: Consensus Clustering
```
Method: PAM
Fixed k: 4
Consensus Clustering: Yes
Bootstrap: 100 iterations
Expected: Stability metrics for each cluster
```

### Test 6: Clinical Correlations
```
IHC markers: ER, PR, HER2, Ki67
Clinical: Age_Years, Tumor_Grade, Tumor_Stage, Lymph_Node_Status
Expected: Statistical tests for associations
```

### Test 7: Survival Analysis
```
IHC markers: ER, PR, HER2, Ki67
Survival Time: OS_Months
Survival Event: OS_Event
Expected: Kaplan-Meier curves by cluster
```

### Test 8: Accessibility Features
```
Color Palette: Colorblind Safe
Font Size: Large (14pt)
High Contrast: Yes
Expected: Enhanced visibility
```

### Test 9: Tumor Preset
```
Tumor Preset: Breast Cancer - Luminal Markers
Apply Preset: Yes
Expected: Recommendations appear
```

### Test 10: Turkish Interface
```
Language: Turkish
Show Natural Summary: Yes
Expected: All text in Turkish
```

### Test 11: NEW - Natural Language Summary
```
Show Natural Summary: Yes
Language: English
Expected: Plain-language panel with:
  - Simple explanation of findings
  - Cluster distribution in everyday language
  - Clinical interpretation
  - Recommended next steps
```

### Test 12: NEW - Contextual Warnings
```
Show Warnings: Yes
Clusters: 8 (too many)
Expected: Warnings about:
  - Overfitting risk
  - Small cluster sizes
  - Poor silhouette values
```

### Test 13: NEW - Statistical Glossary
```
Show Glossary: Yes
Language: English or Turkish
Expected: Quick reference with definitions:
  - Gower Distance
  - Silhouette Width
  - PAM, Hierarchical, etc.
  - Effect sizes (Cramér's V, epsilon-squared)
  - H-score definition
```

### Test 14: Case ID Tracking
```
Case ID: PatientID
Method: PAM
Expected: Medoid table shows actual Patient IDs
```

### Test 15: Export Clusters
```
Export Clusters: Yes
Expected: Notification that assignments are stored
```

### Test 16: Comprehensive (All Features)
```
All categorical markers
All continuous markers
Case ID: PatientID
Method: PAM
Fixed k: 4
All visualizations: Yes
All tables: Yes
Clinical: Age, Grade, Stage
Survival: OS_Months, OS_Event
Show Natural Summary: Yes
Show Warnings: Yes
Show Glossary: Yes
Color Palette: Colorblind
Expected: Full analysis with all panels
```

---

## ✅ Validation Checklist

After running tests, verify:

- [ ] Clusters are interpretable and biologically plausible
- [ ] Silhouette plot shows reasonable cluster quality (>0.5)
- [ ] Heatmap shows clear patterns by cluster
- [ ] Marker summary statistics differ between clusters
- [ ] Association tests show significant markers
- [ ] Clinical correlations (if any) make sense
- [ ] Survival curves (if included) differ between clusters
- [ ] Natural language summary is clear and accurate
- [ ] Warnings appear when expected (overfitting, imbalance)
- [ ] Glossary terms are correctly defined
- [ ] Patient IDs display in medoid table
- [ ] Turkish translation works (if tested)
- [ ] Color palettes apply correctly
- [ ] Export notification appears

---

## 🔬 Advanced Usage

### Compare Clustering Methods

Run same markers with different methods and compare:

| Method | Pros | Expected Result |
|--------|------|-----------------|
| PAM | Robust, interpretable medoids | 4 distinct clusters |
| Hierarchical | Shows relationships | Dendrogram with clear 4-group structure |
| Dimension Reduction | Handles many markers | Similar 4 clusters with MCA/PCA viz |

### Assess Cluster Stability

```
1. Run with consensus clustering (100 bootstraps)
2. Check stability values in consensus table
3. Stability >0.8 = highly stable
4. Stability 0.6-0.8 = moderately stable
5. Stability <0.6 = consider different k or method
```

### Validate Against Ground Truth

```r
# In R (after clustering in jamovi)
library(dplyr)
data <- read.csv("data/ihc_breast_cancer.csv")

# Compare cluster assignments with TrueSubtype
# (Manual comparison - extract cluster assignments from jamovi results)

# Expected:
# - Most Luminal A in one cluster
# - Most HER2+ in another cluster
# - Most TNBC in another cluster
# - Luminal B may split or mix with others
```

---

## 📚 References

### Data Generation

- **Script**: `data-raw/generate_ihc_test_data.R`
- **Seed**: 42 (reproducible)
- **Date**: 2024-01-15

### Breast Cancer Subtypes

- Luminal A: ~40% of breast cancers, best prognosis
- Luminal B: ~20%, intermediate prognosis
- HER2-enriched: ~10-15%, responsive to targeted therapy
- Triple Negative: ~10-20%, aggressive, limited treatment options

### IHC Markers

- **ER/PR**: Hormone receptors, predict endocrine therapy response
- **HER2**: Growth factor receptor, predicts anti-HER2 therapy response
- **Ki67**: Proliferation marker, distinguishes Luminal A from B
- **CK5/6**: Basal marker, elevated in Triple Negative
- **EGFR**: Growth signaling, associated with poor prognosis
- **AR**: Androgen receptor, potential treatment target in TNBC
- **p53**: Tumor suppressor, mutations common in aggressive tumors

---

## 🆘 Troubleshooting

### Issue: No clusters formed

**Possible causes**:
- Not enough cases with complete data
- Too many markers relative to sample size
- All markers constant or near-constant

**Solutions**:
- Use pairwise distance handling for missing data
- Reduce number of markers
- Check data quality (run descriptive statistics first)

### Issue: Many warnings appear

**This is normal** - the dataset includes edge cases to test warning system:
- Some clusters may be small
- Some settings may trigger overfitting warnings
- This validates the warning system works correctly

### Issue: Clusters don't match subtypes perfectly

**This is expected** - clustering is unsupervised:
- Some mixing between subtypes is realistic
- Luminal B overlaps with Luminal A and HER2+
- Real-world data has heterogeneity
- Focus on whether clusters are **interpretable** not perfect

### Issue: Different results on repeated runs

**Check reproducibility settings**:
- Set random seed (default: 42)
- Use same missing data handling strategy
- PAM and hierarchical are deterministic
- Dimension reduction methods may vary slightly

---

## 📞 Support

For questions or issues with the test data:
- GitHub: https://github.com/sbalci/ClinicoPathJamoviModule
- Email: ClinicoPath development team

---

**Version**: 1.0
**Last Updated**: 2024-01-15
**Compatible with**: ClinicoPath ihccluster v2.0.0+