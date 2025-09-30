# Sterlacci 2019 Features - User Guide

## Overview

This guide explains how to use the new Phase 1 features implemented from Sterlacci et al. (2019) study on NSCLC IHC clustering. These features enable replication of their methodology for analyzing immunohistochemistry marker patterns.

**Implemented Features (Phase 1):**
1. Jaccard distance for binary markers
2. Complete linkage hierarchical clustering
3. Bonferroni correction for multiple testing

---

## Feature 1: Jaccard Distance

### When to Use

**Use Jaccard distance when:**
- All your IHC markers are **binary** (Positive/Negative only)
- You want to replicate published studies using Jaccard distance
- You're analyzing tissue microarray (TMA) data with categorical scoring

**Use Gower distance (default) when:**
- You have **mixed data types** (categorical + continuous markers)
- You have ordinal scales (0/1+/2+/3+)
- You have H-scores or percentage positivity values

### Mathematical Background

**Jaccard distance formula:**
```
d(A,B) = 1 - (|A ∩ B| / |A ∪ B|)
```

Where:
- A ∩ B = markers positive in both cases
- A ∪ B = markers positive in either case

**Example:**
```
Case 1: EMA+, CK7+, TTF1-, p63-, CD99+
Case 2: EMA+, CK7-, TTF1+, p63-, CD99+

A ∩ B = 2 (EMA, CD99 both positive)
A ∪ B = 4 (EMA, CK7, TTF1, CD99 at least one positive)
Jaccard similarity = 2/4 = 0.5
Jaccard distance = 1 - 0.5 = 0.5
```

### How to Use in jamovi

1. Load your dataset
2. Navigate to: **Analyses → OncoPath → IHC Analysis → IHC Clustering Analysis**
3. Select your categorical IHC markers
4. Under **Distance Metric**, select **"Jaccard (binary data only)"**
5. Choose **Hierarchical** clustering method
6. Run analysis

**Important Notes:**
- Continuous markers (H-scores, percentages) will be automatically converted to binary using median split
- The analysis will report: "Using Jaccard distance (binary conversion applied)"
- For truly binary data, this is more appropriate than Gower distance

### R Code Example

```r
# Load module
library(ClinicoPath)

# Example: Binary IHC markers
data <- data.frame(
    CaseID = paste0("Case_", 1:50),
    EMA = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
    CK7 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
    TTF1 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
    p63 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE)),
    CD99 = factor(sample(c("Positive", "Negative"), 50, replace = TRUE))
)

# Run clustering with Jaccard distance
results <- ihccluster(
    data = data,
    catVars = c("EMA", "CK7", "TTF1", "p63", "CD99"),
    method = "hierarchical",
    distanceMethod = "jaccard",  # NEW OPTION
    nClusters = 3,
    autoSelectK = TRUE
)
```

---

## Feature 2: Complete Linkage Hierarchical Clustering

### When to Use

**Use Complete linkage when:**
- Replicating Sterlacci 2019 or similar studies
- You want **compact, spherical clusters**
- You expect distinct immunoprofile groups
- Outliers should form separate clusters

**Use Ward linkage (default) when:**
- You want **balanced cluster sizes**
- Minimizing within-cluster variance is priority
- Standard exploratory analysis

### Mathematical Background

**Complete linkage formula:**
```
D(Cluster A, Cluster B) = max{distance(a,b) : a ∈ A, b ∈ B}
```

The distance between two clusters = the **maximum** distance between any two points in the clusters.

**Comparison:**
- **Ward:** Minimizes variance, tends to create equal-sized clusters
- **Complete:** Creates compact clusters, sensitive to outliers
- **Average:** Intermediate between Ward and Complete
- **Single:** Creates chain-like clusters (rarely useful for IHC)

### How to Use in jamovi

1. Load your dataset
2. Navigate to: **Analyses → OncoPath → IHC Analysis → IHC Clustering Analysis**
3. Select your IHC markers
4. Under **Clustering Method**, select **"Hierarchical"**
5. Under **Linkage Method**, select **"Complete (furthest neighbor)"**
6. Run analysis

The dendrogram will show different structure compared to Ward linkage.

### R Code Example

```r
# Compare Ward vs Complete linkage
data <- data.frame(
    EMA = factor(sample(c("Positive", "Negative"), 100, replace = TRUE)),
    CK7 = factor(sample(c("Positive", "Negative"), 100, replace = TRUE)),
    Ki67_Percent = rnorm(100, mean = 30, sd = 15)
)

# Ward linkage (default)
results_ward <- ihccluster(
    data = data,
    catVars = c("EMA", "CK7"),
    contVars = "Ki67_Percent",
    method = "hierarchical",
    linkageMethod = "ward",  # Default
    nClusters = 3
)

# Complete linkage (Sterlacci method)
results_complete <- ihccluster(
    data = data,
    catVars = c("EMA", "CK7"),
    contVars = "Ki67_Percent",
    method = "hierarchical",
    linkageMethod = "complete",  # NEW OPTION
    nClusters = 3
)

# Compare cluster assignments
table(Ward = results_ward$clusters, Complete = results_complete$clusters)
```

### Expected Differences

**Complete linkage typically produces:**
- More outlier cases (cases that don't fit any cluster well)
- Tighter, more homogeneous clusters
- Potentially unbalanced cluster sizes
- Better separation of distinct phenotypes

**Ward linkage typically produces:**
- More balanced cluster sizes
- Fewer outliers
- Gradual transitions between clusters

---

## Feature 3: Bonferroni Correction for Multiple Testing

### When to Use

**Use Bonferroni correction when:**
- Testing **many markers** (≥10) for associations with clusters
- You need **strict control** of Type I error (false positives)
- Publishing results in peer-reviewed journals
- Replicating Sterlacci 2019 methodology

**Use FDR (Benjamini-Hochberg) when:**
- You want **less conservative** correction than Bonferroni
- You have **very many tests** (≥50 markers)
- Discovery phase (exploratory analysis)

**Use no correction when:**
- **Few markers** (<5)
- Purely exploratory analysis
- P-values are descriptive only

### Mathematical Background

**Bonferroni correction:**
```
Adjusted significance threshold = α / n_tests

Example (Sterlacci 2019):
- 70 markers tested
- α = 0.05
- Bonferroni threshold = 0.05 / 70 = 0.000714
```

**Interpretation:**
- With 70 markers and α=0.05, we expect ~3.5 false positives by chance
- Bonferroni ensures overall false positive rate stays at 5%
- Each marker must reach p < 0.000714 to be "significant"

**Adjusted p-values:**
```
p_adjusted = p_raw × n_tests

Example:
- Raw p-value = 0.001
- 20 markers tested
- Adjusted p-value = 0.001 × 20 = 0.020
- Still significant at α = 0.05
```

### How to Use in jamovi

1. Load your dataset
2. Navigate to: **Analyses → OncoPath → IHC Analysis → IHC Clustering Analysis**
3. Select your IHC markers
4. Ensure **Association Tests** is checked
5. Under **Multiple Testing Correction**, select:
   - **Bonferroni** (most conservative)
   - **Benjamini-Hochberg (FDR)** (less conservative)
   - **Holm** (sequential correction)
   - **None** (no correction)
6. Run analysis

**Output:**
The association table will show:
- **p-value** (raw, unadjusted)
- **Adjusted p** (Bonferroni-corrected)
- A note explaining the threshold: "Bonferroni-corrected significance threshold: p < 0.002500 (α=0.05 / 20 markers)"

### R Code Example

```r
# Large marker panel requiring correction
data <- data.frame(
    # Create 30 IHC markers
    matrix(
        sample(c("Positive", "Negative"), 100 * 30, replace = TRUE),
        nrow = 100,
        ncol = 30,
        dimnames = list(NULL, paste0("Marker_", 1:30))
    )
)

# Convert to factors
data <- as.data.frame(lapply(data, factor))

# Run with Bonferroni correction
results <- ihccluster(
    data = data,
    catVars = paste0("Marker_", 1:30),
    method = "pam",
    nClusters = 3,
    autoSelectK = FALSE,
    associationTests = TRUE,
    multipleTestingCorrection = "bonferroni"  # NEW OPTION
)

# Extract association results
associations <- results$associationTests

# View markers significant after correction
significant <- associations[associations$p_adjusted < 0.05, ]
print(significant)
```

### Interpreting Results

**Example Output:**

| Marker | Test | p-value | Adjusted p | Effect Size |
|--------|------|---------|------------|-------------|
| CK7 | Chi-square | 0.0001 | **0.003** | 0.45 (strong) |
| TTF1 | Chi-square | 0.0002 | **0.006** | 0.42 (strong) |
| EMA | Chi-square | 0.002 | 0.060 | 0.28 (moderate) |
| CD99 | Chi-square | 0.15 | 1.000 | 0.10 (weak) |

**Note:** Bonferroni-corrected significance threshold: p < 0.0017 (α=0.05 / 30 markers)

**Interpretation:**
- **CK7 and TTF1**: Significantly associated with clusters even after correction (adjusted p < 0.05)
- **EMA**: Raw p-value suggests association (p=0.002), but not significant after correction (adjusted p=0.060)
- **CD99**: No association detected

**Clinical Meaning:**
- CK7 and TTF1 are **robust cluster-defining markers**
- EMA shows a trend but may be a **false positive**
- CD99 is **not associated** with the clustering pattern

---

## Combining All Features: Sterlacci 2019 Replication

To fully replicate the Sterlacci et al. 2019 methodology:

### jamovi Workflow

1. Load your NSCLC IHC dataset
2. Select all 70+ markers (or your marker panel)
3. **Clustering settings:**
   - Method: **Hierarchical**
   - Distance Metric: **Jaccard (binary data only)**
   - Linkage Method: **Complete (furthest neighbor)**
   - Auto-select k: **Yes** (or manually set to 3-4)
4. **Analysis options:**
   - Association Tests: **✓ Checked**
   - Multiple Testing Correction: **Bonferroni**
5. Run analysis

### Expected Results (Sterlacci-like)

**Cluster 1: ACA-like phenotype**
- CK7+, TTF1+, EGFR+, E-cadherin+
- CK5/6-, p63-, CD44-

**Cluster 2: SCC-like phenotype**
- CK5/6+, p63+, CD44+, SOX2+
- CK7-, TTF1-, EGFR-

**Cluster 3: Immune signature**
- High CD8+ TIL
- Low CD4/CD8 ratio
- PD1+, PDL1+
- Mixed tumor marker expression

### R Code: Full Replication

```r
library(ClinicoPath)

# Load NSCLC dataset (example structure)
data <- read.csv("nsclc_ihc_data.csv")

# Full Sterlacci 2019 replication
results <- ihccluster(
    data = data,

    # Structural markers
    catVars = c("CK7", "CK5_6", "TTF1", "p63", "CD44", "E_cadherin",
                "EGFR", "SOX2", "CD56", "Synaptophysin", "Chromogranin"),

    # Immune markers
    catVars = c(catVars, "CD4", "CD8", "PD1", "PDL1", "FoxP3",
                "Granzyme_B", "TIA1", "CD20", "CD68"),

    # Proliferation
    contVars = c("Ki67_Percent"),

    # Sterlacci 2019 settings
    method = "hierarchical",
    distanceMethod = "jaccard",           # Binary distance
    linkageMethod = "complete",           # Complete linkage
    nClusters = 3,
    autoSelectK = TRUE,

    # Statistical rigor
    associationTests = TRUE,
    multipleTestingCorrection = "bonferroni",  # Conservative correction

    # Visualization
    showHeatmap = TRUE,
    showDendrogram = TRUE,
    showSilhouette = TRUE
)

# Extract key results
print(results$clusterSizes)
print(results$clusterProfiles)
print(results$associationTests[results$associationTests$p_adjusted < 0.05, ])
```

---

## Troubleshooting

### Error: "Package 'proxy' required for Jaccard distance"

**Solution:** Install the proxy package:
```r
install.packages("proxy")
```

### Warning: "Jaccard distance requires binary data"

**Cause:** You selected Jaccard distance but have continuous markers.

**Solution:**
- Option 1: Use Gower distance instead
- Option 2: The function will automatically convert continuous markers to binary (median split) and proceed

### All p-values become 1.000 after Bonferroni correction

**Cause:** Too many markers tested, Bonferroni is very conservative.

**Solution:** Try less conservative correction:
- Use **FDR (Benjamini-Hochberg)** instead
- Or reduce number of markers to most clinically relevant ones

### Complete linkage produces very unbalanced clusters

**This is expected behavior.** Complete linkage emphasizes cluster compactness over balance.

**Options:**
- If unbalanced clusters are problematic, use Ward linkage
- If you want to identify outliers, complete linkage is working correctly

---

## References

**Primary Reference:**
Sterlacci W, Savic S, Schmid T, Oberaigner W, Auberger J, Fiegl M. Tissue microarray based analysis of immunohistochemical expression patterns of molecular targets in NSCLC: correlation with patient outcome and comparison between adenocarcinoma and squamous cell carcinoma. *Histol Histopathol*. 2019.

**Implementation:**
ClinicoPath R Package for jamovi. https://github.com/sbalci/ClinicoPathJamoviModule

**Additional Documentation:**
- Full feature analysis: `STERLACCI_2019_FEATURE_ANALYSIS.md`
- IHC prediction workflow: `IHC_PREDICTION_PATHOLOGIST_GUIDE.md`
- Diagnostic clustering guide: `DIAGNOSTIC_CLUSTERING_GUIDE.md`

---

## Coming in Phase 2

The following features from Sterlacci 2019 will be implemented in Phase 2:

1. **Reproducibility testing** - Random split + Cohen's kappa validation
2. **Supervised clustering** - Clustering within known diagnosis groups
3. **CD4/CD8 ratio calculation** - Automatic ratio computation
4. **Binary heatmap visualization** - Yellow/green color scheme

Stay tuned for updates!

---

**Document Version:** 1.0
**Date:** 2025-01-15
**Implementation:** Phase 1 Complete
