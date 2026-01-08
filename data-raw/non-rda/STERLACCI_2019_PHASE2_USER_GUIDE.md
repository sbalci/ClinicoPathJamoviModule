# IHC Clustering Phase 2 User Guide: Reproducibility & Supervised Clustering

**ClinicoPath jamovi Module**
**Version:** 2.0.0
**Date:** 2025-09-30
**Reference:** Sterlacci et al. (2019). *Histol Histopathol.*

---

## Table of Contents

1. [Overview](#overview)
2. [Phase 2 Features](#phase-2-features)
3. [Reproducibility Testing](#reproducibility-testing)
4. [Supervised Clustering](#supervised-clustering)
5. [Step-by-Step Examples](#step-by-step-examples)
6. [Interpreting Results](#interpreting-results)
7. [Best Practices](#best-practices)
8. [Troubleshooting](#troubleshooting)

---

## Overview

Phase 2 of the Sterlacci 2019 implementation adds advanced validation and stratified analysis features to IHC clustering:

**Two new features:**
1. **Reproducibility Testing** - Random split validation with Cohen's kappa
2. **Supervised Clustering** - Clustering within predefined diagnostic groups

These features complement Phase 1 capabilities (Jaccard distance, complete linkage, Bonferroni correction) to provide a complete implementation of the Sterlacci et al. methodology.

---

## Phase 2 Features

### 1. Reproducibility Testing

**Purpose:** Assess cluster stability and reliability

**Method:** Random split validation
- Dataset randomly split 50/50
- Independent clustering on each half
- Agreement measured using Cohen's kappa
- Repeated N times (default: 10 splits)

**Output:**
- Cohen's kappa (κ) for each cluster
- Mean κ and standard deviation
- 95% confidence intervals
- Interpretation (Poor/Fair/Moderate/Substantial/Almost Perfect)

**When to use:**
- Publishing results requiring validation
- Exploring whether clusters are robust or data-dependent
- Comparing stability across different clustering methods
- Need objective cluster quality metric

---

### 2. Supervised Clustering

**Purpose:** Explore marker expression patterns within known diagnostic groups

**Method:** Stratified clustering
- Clustering performed separately within each diagnostic group
- Identifies subgroups within diagnoses
- Reveals intra-diagnostic heterogeneity

**Output:**
- Summary table: Cases, clusters, silhouette scores per group
- Detailed HTML: Cluster sizes and characteristics per group
- Status warnings for groups with insufficient data

**When to use:**
- Known diagnosis available
- Exploring heterogeneity within diagnostic categories
- Comparing marker patterns across diagnoses
- Quality control (verify expected patterns within diagnosis)

---

## Reproducibility Testing

### What is Cohen's Kappa?

Cohen's kappa (κ) measures agreement between two clustering runs on independent data splits:

**Formula:**
```
κ = (Observed Agreement - Expected Agreement) / (1 - Expected Agreement)
```

**Interpretation (Landis & Koch, 1977):**

| Kappa Range | Interpretation | Cluster Quality |
|-------------|---------------|-----------------|
| κ < 0.21    | Poor          | Unstable clusters |
| 0.21 ≤ κ < 0.41 | Fair      | Low reproducibility |
| 0.41 ≤ κ < 0.61 | Moderate  | Acceptable |
| 0.61 ≤ κ < 0.81 | Substantial | Good reproducibility |
| 0.81 ≤ κ ≤ 1.00 | Almost Perfect | Excellent reproducibility |

### How It Works

1. **Random Split:**
   - Dataset divided into two equal random halves (Group A and Group B)
   - No overlap between groups

2. **Independent Clustering:**
   - Same clustering method applied to both groups separately
   - No information shared between clustering runs

3. **Cluster Matching:**
   - Clusters matched based on marker profile similarity
   - Best matches identified using marker expression patterns

4. **Agreement Calculation:**
   - For each cluster, proportion of cases calculated in both groups
   - Observed agreement: How often same cluster appears in both groups
   - Expected agreement: How often agreement would occur by chance
   - Kappa: Agreement beyond chance

5. **Repetition:**
   - Process repeated N times (default: 10) with different random splits
   - Mean κ and SD calculated for each cluster

### jamovi Interface

**Options:**

**Test Cluster Reproducibility** (checkbox)
- Enable reproducibility testing
- Default: Off (due to computational cost)

**Number of Random Splits** (integer)
- Range: 1-100
- Default: 10
- More splits → more reliable estimates, but slower

**Output Table:**

`Cluster Reproducibility (Random Split Validation)`

| Column | Description |
|--------|-------------|
| `cluster` | Cluster identifier (e.g., "Cluster_1") |
| `mean_kappa` | Average Cohen's kappa across all splits |
| `sd_kappa` | Standard deviation of kappa values |
| `ci_lower` | Lower bound of 95% CI |
| `ci_upper` | Upper bound of 95% CI |
| `interpretation` | Verbal interpretation of kappa |
| `n_splits` | Number of random splits performed |

### Example Interpretation

**Example Result:**
```
Cluster         Mean κ   SD     95% CI           Interpretation
Cluster_1       0.72    0.08   [0.64, 0.80]     Substantial
Cluster_2       0.65    0.11   [0.54, 0.76]     Substantial
Cluster_3       0.45    0.15   [0.30, 0.60]     Moderate
```

**What this means:**
- **Cluster_1** (κ = 0.72): Highly reproducible. This cluster represents a stable, biologically meaningful subgroup.
- **Cluster_2** (κ = 0.65): Good reproducibility. Reliable cluster.
- **Cluster_3** (κ = 0.45): Moderate reproducibility. May represent transitional phenotype or borderline cases.

**Action:**
- Trust Clusters 1 and 2 for clinical interpretation
- Investigate Cluster 3 further: Check marker profiles, outliers, borderline cases

---

## Supervised Clustering

### What is Supervised Clustering?

Supervised clustering performs clustering **within** each known diagnostic group separately:

**Example:**
- **Dataset:** 100 NSCLC cases
  - 50 Adenocarcinoma
  - 50 Squamous Cell Carcinoma

- **Standard clustering:** All 100 cases clustered together
- **Supervised clustering:**
  - Adenocarcinoma cases (n=50) clustered separately → finds subgroups within adenocarcinoma
  - Squamous Cell cases (n=50) clustered separately → finds subgroups within squamous cell

### Why Use Supervised Clustering?

**1. Explore Intra-Diagnostic Heterogeneity**
- Known diagnoses often have subtypes
- Example: Luminal A breast cancer may have high-Ki67 vs low-Ki67 subgroups

**2. Quality Control**
- Verify expected marker patterns within each diagnosis
- Detect unexpected subgroups that may indicate misdiagnosis

**3. Stratified Analysis**
- Compare marker patterns across diagnoses
- Understand how markers behave differently in each diagnostic context

**4. Clinical Utility**
- More relevant for pathologists: "Within adenocarcinomas, are there clinically relevant subgroups?"
- vs. "What groups exist in the whole dataset?" (which may just separate diagnoses)

### jamovi Interface

**Options:**

**Supervised Clustering** (checkbox)
- Enable supervised clustering
- Default: Off

**Grouping Variable for Supervised Clustering** (variable selector)
- Select the variable defining diagnostic groups
- Must be categorical (nominal or ordinal)
- Example variables: Diagnosis, Histotype, Subtype

**Output Tables:**

**1. Supervised Clustering Summary**

| Column | Description |
|--------|-------------|
| `group` | Diagnostic group name |
| `n_cases` | Number of cases in this group |
| `n_clusters` | Number of clusters found in this group |
| `avg_silhouette` | Average silhouette score (cluster quality) |
| `status` | "Success" or warning message |

**2. Supervised Clustering Details (HTML)**

Detailed report for each diagnostic group:
- Group name
- Number of cases
- Number of clusters identified
- Average silhouette score
- Table of cluster sizes

**Warning Messages:**
- `"Skipped: Too few cases (need X)"` - Group has insufficient cases for clustering

### Minimum Sample Sizes

**Clustering requires:**
- At least `2 × k` cases per group
- Where `k` = number of clusters requested

**Default (k=3):**
- Need at least 6 cases per diagnostic group

**Recommendations:**
- Small groups (n < 10): May produce unreliable clusters
- Adequate groups (n ≥ 20): Reliable clustering possible
- Large groups (n ≥ 50): High confidence in results

---

## Step-by-Step Examples

### Example 1: Reproducibility Testing

**Scenario:** NSCLC cohort with 100 cases, binary IHC markers

**Steps:**

1. **Open jamovi** → Analyses → ClinicoPath → IHC Analysis → IHC Clustering Analysis

2. **Select Variables:**
   - Categorical IHC Markers: `CK7`, `TTF1`, `p63`, `CK5_6`
   - Continuous IHC Markers: `Ki67_Percent`
   - Case ID: `PatientID`

3. **Clustering Options:**
   - Clustering Method: `Hierarchical (Ward) on Gower`
   - Distance Metric: `Jaccard (binary data only)`
   - Number of Clusters: `3`
   - Automatically Select Optimal k: `Uncheck`

4. **Phase 2 Options:**
   - ✅ **Test Cluster Reproducibility**
   - Number of Random Splits: `10`

5. **Output Options:**
   - ✅ Silhouette Plot
   - ✅ Expression Heatmap
   - ✅ Cluster Quality Metrics
   - ✅ Association Tests
   - Multiple Testing Correction: `Bonferroni`

6. **Run Analysis**

7. **Interpret Results:**

   Check **Cluster Reproducibility** table:
   ```
   Cluster    Mean κ   Interpretation
   Cluster_1  0.78     Substantial
   Cluster_2  0.71     Substantial
   Cluster_3  0.55     Moderate
   ```

   **Conclusion:** Clusters 1 and 2 are highly reproducible. Cluster 3 has moderate stability and may represent borderline cases.

---

### Example 2: Supervised Clustering

**Scenario:** Breast cancer cohort with known molecular subtypes

**Steps:**

1. **Open jamovi** → Analyses → ClinicoPath → IHC Analysis → IHC Clustering Analysis

2. **Select Variables:**
   - Categorical IHC Markers: `ER_Status`, `PR_Status`, `HER2_IHC`
   - Continuous IHC Markers: `Ki67_Percent`, `AR_Hscore`
   - Case ID: `PatientID`

3. **Clustering Options:**
   - Clustering Method: `PAM (k-medoids) on Gower`
   - Distance Metric: `Gower (handles mixed data)`
   - Number of Clusters: `2`
   - Scale Continuous Variables: `✓`

4. **Phase 2 Options:**
   - ✅ **Supervised Clustering**
   - Grouping Variable: `TrueSubtype` (Luminal_A, Luminal_B, HER2_Positive, Triple_Negative)

5. **Output Options:**
   - ✅ Expression Heatmap
   - ✅ Cluster Quality Metrics

6. **Run Analysis**

7. **Interpret Results:**

   Check **Supervised Clustering Summary**:
   ```
   Group               N Cases  N Clusters  Avg Silhouette  Status
   Luminal_A          60        2           0.42            Success
   Luminal_B          40        2           0.38            Success
   HER2_Positive      25        2           0.35            Success
   Triple_Negative    25        2           0.51            Success
   ```

   Check **Supervised Clustering Details** (HTML output):
   - Luminal_A: 2 clusters identified
     - Cluster 1: 35 cases (likely low-Ki67)
     - Cluster 2: 25 cases (likely high-Ki67)

   **Conclusion:** Each molecular subtype contains heterogeneous subgroups. Triple_Negative cases show highest intra-subtype clustering (Sil = 0.51), suggesting clear phenotypic differences.

---

### Example 3: Combined Phase 2 Features

**Scenario:** Validate clustering and explore within-diagnosis heterogeneity simultaneously

**Steps:**

1. **Select Variables** (as in previous examples)

2. **Phase 2 Options:**
   - ✅ **Test Cluster Reproducibility**
   - Number of Random Splits: `10`
   - ✅ **Supervised Clustering**
   - Grouping Variable: `Diagnosis`

3. **Run Analysis**

4. **Interpret Results:**

   **First, check reproducibility:**
   - Are unsupervised clusters stable across random splits?
   - Kappa values indicate which clusters are robust

   **Then, check supervised clustering:**
   - Within each diagnosis, are there meaningful subgroups?
   - Compare supervised subgroups to unsupervised clusters

   **Integration:**
   - High kappa + clear supervised subgroups → Strong evidence for biologically meaningful clusters
   - Low kappa but clear supervised subgroups → Diagnosis-specific patterns exist, but overall clustering is unstable
   - High kappa but no supervised subgroups → Clustering mainly separates diagnoses, not subgroups within diagnoses

---

## Interpreting Results

### Reproducibility Testing Results

#### High Reproducibility (κ > 0.61)

**What it means:**
- Clusters are stable and consistent
- Clustering is not highly dependent on random sampling
- Clusters likely represent true biological subgroups

**Action:**
- ✅ Trust these clusters for clinical interpretation
- ✅ Use cluster assignments for downstream analysis
- ✅ Report clusters in publications with confidence

**Example:**
```
Cluster_1: Mean κ = 0.78 (SD = 0.06)
→ Highly reliable. This cluster represents a distinct, stable phenotype.
```

---

#### Moderate Reproducibility (0.41 ≤ κ ≤ 0.61)

**What it means:**
- Clusters have acceptable stability
- Some cases may shift between clusters in different splits
- Likely represents transitional or borderline phenotypes

**Action:**
- ⚠️ Use with caution
- Investigate which cases are unstable (check silhouette scores)
- Consider merging with nearby clusters
- Increase sample size if possible

**Example:**
```
Cluster_3: Mean κ = 0.48 (SD = 0.12)
→ Moderate stability. Check for outliers or borderline cases.
```

---

#### Low Reproducibility (κ < 0.41)

**What it means:**
- Clusters are unstable and unreliable
- Clustering is highly sensitive to random sampling
- May indicate:
  - Insufficient sample size
  - Too many clusters requested (over-clustering)
  - Noisy data
  - Truly homogeneous population (no real clusters)

**Action:**
- ❌ Do not trust these clusters for interpretation
- Reduce number of clusters (k)
- Check for data quality issues
- Consider using different clustering method
- Increase sample size

**Example:**
```
Cluster_2: Mean κ = 0.18 (SD = 0.21)
→ Poor reproducibility. This cluster is unreliable and should not be interpreted.
```

---

### Supervised Clustering Results

#### Successful Clustering

**Example:**
```
Group: Luminal_A
N Cases: 60
N Clusters: 2
Avg Silhouette: 0.42
Status: Success

Cluster Sizes:
- Cluster 1: 38 cases
- Cluster 2: 22 cases
```

**Interpretation:**
- Luminal_A breast cancers split into two subgroups
- Good separation (Sil = 0.42 > 0.25)
- Inspect marker profiles to understand biological meaning
  - Check Ki67 levels
  - Check hormone receptor levels
  - Compare survival outcomes

---

#### Skipped Due to Small Sample

**Example:**
```
Group: Test_Cases
N Cases: 5
Status: Skipped: Too few cases (need 6)
```

**Action:**
- Exclude this group from supervised analysis
- Note in limitations section
- Consider combining with similar diagnostic group if biologically justified

---

#### Poor Cluster Separation

**Example:**
```
Group: Triple_Negative
N Cases: 25
N Clusters: 2
Avg Silhouette: 0.12
Status: Success
```

**Interpretation:**
- Clustering technically succeeded
- But clusters have poor separation (Sil = 0.12 < 0.25)
- May indicate:
  - Triple_Negative cases are relatively homogeneous
  - Markers used do not discriminate subgroups well
  - Need different markers for this diagnosis

**Action:**
- Report low silhouette score
- Consider whether subgroups are clinically meaningful
- May try different number of clusters or different markers

---

## Best Practices

### Reproducibility Testing

**1. When to Use:**
- ✅ Publishing results
- ✅ Clinical validation studies
- ✅ Comparing clustering methods
- ✅ Exploring optimal number of clusters

**2. Number of Splits:**
- **Quick exploration:** 5 splits
- **Standard analysis:** 10 splits
- **Publication quality:** 20-50 splits
- **Comprehensive validation:** 100 splits

**3. Sample Size Requirements:**
- **Minimum:** 40 cases total
- **Adequate:** 100 cases
- **Recommended:** 200+ cases

**4. Interpreting Results:**
- Focus on mean kappa, not individual split values
- Check SD: High SD indicates inconsistent reproducibility
- Compare kappa across clusters: Which clusters are most stable?

---

### Supervised Clustering

**1. When to Use:**
- ✅ Known diagnosis available
- ✅ Exploring intra-diagnostic heterogeneity
- ✅ Quality control
- ✅ Stratified marker analysis

**2. Grouping Variable Selection:**
- Use well-established diagnostic categories
- Avoid creating too many small groups
- Each group should have ≥ 20 cases ideally

**3. Number of Clusters:**
- Start with k=2 for exploratory analysis
- Increase k cautiously based on silhouette scores
- Within-group clusters will have lower separation than across-group

**4. Interpreting Results:**
- Compare supervised clusters to known subtypes (if available)
- Look for consistency across diagnostic groups
- High-quality supervised clusters: Sil > 0.30 within group

---

### Combining Both Features

**Workflow:**

1. **Start with reproducibility testing on full dataset:**
   - Establish which unsupervised clusters are stable
   - Identify optimal k

2. **Then perform supervised clustering:**
   - Explore heterogeneity within diagnostic groups
   - Compare to unsupervised clusters

3. **Integration:**
   - Do supervised subgroups match unsupervised clusters?
   - If yes: Strong biological signal
   - If no: Clustering may be separating diagnoses, not subgroups

---

## Troubleshooting

### Issue 1: Low Kappa Values for All Clusters

**Symptoms:**
- All clusters show κ < 0.40
- High standard deviation in kappa

**Possible Causes:**
1. **Insufficient sample size:**
   - Solution: Increase N (need ≥ 100 cases)

2. **Too many clusters:**
   - Solution: Reduce k. Try k-1 or k-2

3. **Noisy data:**
   - Solution: Check data quality, missing values
   - Remove uninformative markers

4. **No true clusters exist:**
   - Solution: Accept that population may be homogeneous
   - Consider different markers or clinical grouping

---

### Issue 2: Supervised Clustering Skips Most Groups

**Symptoms:**
- Most groups show "Skipped: Too few cases"

**Cause:**
- Diagnostic groups are too small

**Solutions:**
1. Combine related diagnostic categories
2. Reduce number of clusters (k)
3. Collect more data
4. Use unsupervised clustering instead

---

### Issue 3: Reproducibility Test is Very Slow

**Symptoms:**
- Analysis takes many minutes to complete

**Cause:**
- High number of splits × large dataset

**Solutions:**
1. Reduce number of splits temporarily (e.g., 5 instead of 10)
2. Use faster clustering method (PAM instead of hierarchical)
3. Reduce number of markers
4. Run on subset of data first for exploration

---

### Issue 4: Inconsistent Kappa Across Splits

**Symptoms:**
- High SD in kappa values
- Wide confidence intervals

**Possible Causes:**
1. Small sample size → random variation
2. Outliers influencing some splits but not others
3. Borderline cases shifting between clusters

**Solutions:**
1. Increase number of splits (averages out variation)
2. Remove outliers (check silhouette < 0)
3. Increase sample size
4. Inspect which cases are unstable

---

## References

**Primary Reference:**
Sterlacci W, et al. (2019). Tissue microarray based analysis of immunohistochemical expression patterns of molecular targets in NSCLC. *Histol Histopathol*.

**Cohen's Kappa:**
Landis JR, Koch GG. (1977). The measurement of observer agreement for categorical data. *Biometrics*, 33(1), 159-174.

**Clustering Validation:**
Rousseeuw PJ. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. *J Comput Appl Math*, 20, 53-65.

---

## Additional Resources

**Related Documentation:**
- `STERLACCI_2019_FEATURE_ANALYSIS.md` - Complete technical specification
- `STERLACCI_2019_USER_GUIDE.md` - Phase 1 user guide (Jaccard, complete linkage, Bonferroni)
- `JACCARD_BINARY_CONVERSION_GUIDE.md` - Guide for binary data conversion
- `PHASE_1_IMPLEMENTATION_SUMMARY.md` - Phase 1 technical summary

**Support:**
- GitHub Issues: https://github.com/sbalci/ClinicoPathJamoviModule/issues
- jamovi Forum: https://forum.jamovi.org

---

**Document Version:** 1.0
**Date:** 2025-09-30
**Related Feature:** Phase 2 Implementation (Reproducibility Testing, Supervised Clustering)
