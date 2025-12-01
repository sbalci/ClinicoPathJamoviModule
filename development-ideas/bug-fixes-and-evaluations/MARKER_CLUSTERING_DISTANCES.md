# IHC Marker Clustering Distance Metrics

## Overview

The `ihccluster` function now supports **9 different distance metrics** for marker-level clustering, allowing you to analyze co-expression patterns and redundancy among immunohistochemistry markers.

---

## Distance Metrics by Data Type

### For Categorical IHC Markers (pos/neg, 0/1/2/3, ordinal)

#### 1. **Chi-squared Distance** ⭐ (Default)
- **Method:** `chisquared`
- **Best for:** General categorical associations
- **How it works:** Uses chi-squared statistic from contingency tables
- **Interpretation:** Higher chi-squared = more different = higher distance
- **Statistical test:** Chi-squared test with Cramér's V effect size
- **When to use:**
  - Default choice for categorical markers
  - When you want statistically principled distances
  - Multi-level ordinal data (0/1/2/3)

**Example:** CK7 (pos/neg) vs CK20 (pos/neg) → Chi-squared tests independence

---

#### 2. **Jaccard Distance**
- **Method:** `jaccard`
- **Best for:** Binary markers, sparse data
- **Formula:** `distance = 1 - (intersection / union)`
- **Range:** 0 (identical) to 1 (completely different)
- **How it works:** Measures overlap, ignoring double-negatives
- **When to use:**
  - Binary markers (pos/neg only)
  - When negative-negative matches are not informative
  - Comparing presence/absence patterns

**Example:** ER+ vs PR+ cases → High Jaccard similarity means often co-expressed

---

#### 3. **Hamming Distance**
- **Method:** `hamming`
- **Best for:** Simple mismatch counting
- **Formula:** `distance = (number of mismatches) / (total comparisons)`
- **Range:** 0 (identical) to 1 (always different)
- **How it works:** Counts positions where markers differ
- **When to use:**
  - Quick, intuitive distance measure
  - Multi-level categorical data
  - When all differences are equally important

**Example:** p53 (wild-type/mutant) vs Ki67 (low/high) → 30% cases differ

---

#### 4. **Cramér's V Distance**
- **Method:** `cramer`
- **Best for:** Normalized association strength
- **Formula:** `distance = 1 - Cramér's V`
- **Range:** 0 (perfectly associated) to 1 (independent)
- **How it works:** Normalizes chi-squared by sample size and table dimensions
- **When to use:**
  - Comparing associations across different table sizes
  - When you need normalized effect sizes
  - Tables with different numbers of categories

**Example:** CK7 (3 levels) vs TTF1 (2 levels) → Normalized comparison

---

### For Continuous IHC Markers (H-scores, % positivity)

#### 5. **Euclidean Distance** ⭐ (Recommended)
- **Method:** `euclidean`
- **Best for:** Standard geometric distance
- **Formula:** `sqrt(sum((x_i - y_i)^2))`
- **How it works:** Straight-line distance in multidimensional space
- **Pre-processing:** Automatically scales variables (z-score)
- **When to use:**
  - Default for continuous markers
  - H-scores, % positivity values
  - When variance matters (penalizes large deviations)

**Example:** Ki67 H-score vs p53 H-score → Geometric distance between patterns

---

#### 6. **Manhattan Distance** (L1 norm)
- **Method:** `manhattan`
- **Best for:** Outlier-resistant distance
- **Formula:** `sum(|x_i - y_i|)`
- **How it works:** Sum of absolute differences (taxi-cab distance)
- **Pre-processing:** Automatically scales variables
- **When to use:**
  - When data has outliers
  - More robust than Euclidean
  - Interpretable as "total absolute difference"

**Example:** CD4 % vs CD8 % → Less sensitive to extreme values

---

#### 7. **Correlation Distance**
- **Method:** `correlation`
- **Best for:** Pattern similarity (ignoring magnitude)
- **Formula:** `distance = 1 - |correlation|`
- **Range:** 0 (perfectly correlated) to 2 (perfectly anti-correlated)
- **How it works:** Measures linear relationship strength
- **When to use:**
  - Focus on co-variation pattern, not absolute values
  - When markers have different scales
  - Identifying markers that vary together

**Example:** ER% vs PR% → Often positively correlated in breast cancer

---

### For Mixed Data (Categorical + Continuous)

#### 8. **Mutual Information Distance** ⭐ (Advanced)
- **Method:** `mutual_info`
- **Best for:** Non-linear relationships, any data type
- **How it works:**
  - Measures shared information between markers
  - Uses entropy-based calculations
  - Discretizes continuous variables (5 bins)
- **Range:** 0 (independent) to 1 (perfectly dependent)
- **When to use:**
  - Non-linear relationships
  - Mixed categorical and continuous markers
  - Model-free approach (no distribution assumptions)

**Example:** HER2 status (categorical) vs Ki67 % (continuous) → Information overlap

---

#### 9. **Mixed Distance** (Automatic)
- **Method:** `mixed`
- **Best for:** Automatic handling of mixed data
- **How it works:**
  - Categorical pairs → Chi-squared distance
  - Continuous pairs → Correlation distance
  - Mixed pairs → Eta-squared (ANOVA-based)
- **When to use:**
  - Panel with both categorical and continuous markers
  - Want automatic method selection
  - Don't want to manually specify distance type

**Example:** Panel with ER (pos/neg), PR (pos/neg), Ki67 (%), HER2 H-score

---

## Comparison Table

| Distance Metric | Data Type | Outlier Robust | Statistical Test | Normalized | Computational Cost |
|----------------|-----------|----------------|------------------|------------|-------------------|
| Chi-squared | Categorical | Moderate | Yes (χ²) | No | Medium |
| Jaccard | Binary | Yes | No | Yes | Low |
| Hamming | Categorical | Yes | No | Yes | Low |
| Cramér's V | Categorical | Moderate | Yes (χ²) | Yes | Medium |
| Euclidean | Continuous | No | No | Auto-scaled | Low |
| Manhattan | Continuous | Yes | No | Auto-scaled | Low |
| Correlation | Continuous | Moderate | Yes (cor.test) | Yes | Low |
| Mutual Info | Any | Yes | No | Yes | High |
| Mixed | Any | Moderate | Varies | Partial | Medium |

---

## Recommendations by Use Case

### 1. **Differential Diagnosis Panels** (e.g., lung adenocarcinoma vs squamous)
- **Recommended:** Chi-squared or Cramér's V
- **Why:** Categorical markers (TTF1+/-, p40+/-), need statistical tests
- **Output:** Identifies which markers provide redundant information

### 2. **Breast Cancer IHC Panel** (ER, PR, HER2, Ki67)
- **Recommended:** Mixed distance
- **Why:** Mix of binary (ER/PR/HER2) and continuous (Ki67 %)
- **Output:** Shows ER-PR cluster, Ki67-HER2 relationships

### 3. **Immune Cell Quantification** (CD3, CD4, CD8, PD-L1 percentages)
- **Recommended:** Euclidean or Manhattan
- **Why:** All continuous percentages
- **Output:** CD4-CD8 relationship, PD-L1 associations

### 4. **Multi-level Ordinal Markers** (intensity scoring 0/1+/2+/3+)
- **Recommended:** Chi-squared or Hamming
- **Why:** Ordinal categories with multiple levels
- **Output:** Which intensity scores tend to coincide

### 5. **Sparse Binary Markers** (rare marker positivity)
- **Recommended:** Jaccard
- **Why:** Ignores frequent double-negatives, focuses on co-positivity
- **Output:** Which rare markers co-occur

### 6. **Exploratory Analysis** (unknown relationships)
- **Recommended:** Mutual Information
- **Why:** Captures non-linear relationships, no assumptions
- **Output:** Model-free marker associations

---

## Interpretation Guidelines

### Reading the Dendrogram

1. **Height (Y-axis):** Distance at which markers merge
   - Low height = markers are similar (cluster early)
   - High height = markers are different (cluster late)

2. **Branches:** Show hierarchical relationships
   - Markers on same branch = co-expressed
   - Distant branches = independent markers

3. **Significance Threshold** (red dashed line):
   - Below line = statistically significant clustering
   - Above line = may be due to chance

4. **Colored Rectangles:**
   - Highlight identified marker groups
   - Same color = redundant/co-expressed markers

### Association Test Interpretations

| p-value | Cramér's V | Interpretation |
|---------|-----------|----------------|
| < 0.001 | > 0.5 | Very strong association |
| < 0.01 | 0.3-0.5 | Strong association |
| < 0.05 | 0.1-0.3 | Moderate association |
| ≥ 0.05 | < 0.1 | Weak/No association |

---

## About C4.5

**C4.5 is NOT a distance metric** - it's a machine learning algorithm for building decision trees (developed by Ross Quinlan).

If you're interested in **entropy-based measures** (which C4.5 uses), you can:
- Use **Mutual Information distance** (already implemented) - this IS based on entropy
- Information Gain = what C4.5 uses to split trees
- Could be adapted to distance, but MI is more standard

---

## Technical Notes

### Distance vs. Similarity

All metrics are converted to **distances**:
- Distance = 0 → markers are identical/perfectly associated
- Distance = 1 → markers are completely different/independent

Some metrics naturally measure similarity (correlation, Cramér's V) and are converted:
```
distance = 1 - similarity
```

### Normalization

- **Euclidean & Manhattan:** Auto-scaled using z-scores
- **Chi-squared:** Normalized by maximum chi-squared in matrix
- **Cramér's V:** Already normalized by design
- **Mutual Information:** Normalized by maximum entropy

### Missing Data

All methods handle missing data via:
- Pairwise deletion (uses available cases for each pair)
- No imputation is performed
- Distances computed only on valid observations

---

## References

1. **Chi-squared clustering:** Greenacre (2017) "Correspondence Analysis in Practice"
2. **Jaccard & Dice:** Dice (1945), Jaccard (1912) - ecological similarity
3. **Hamming distance:** Hamming (1950) - information theory
4. **Cramér's V:** Cramér (1946) - association measure
5. **Mutual Information:** Shannon (1948) - information theory
6. **Distance metrics:** Deza & Deza (2009) "Encyclopedia of Distances"

---

## Implementation Details

**File locations:**
- Options: `jamovi/ihccluster.a.yaml:298-321`
- Backend: `R/ihccluster.b.R:3594-3906`
- Output tables: `jamovi/ihccluster.r.yaml:622-731`

**Function calls:**
```r
# Chi-squared
.computeChiSquaredDistance(df, catVars)

# Jaccard
.computeJaccardDistance(df, catVars)

# Hamming
.computeHammingDistance(df, catVars)

# Cramér's V
.computeCramerDistance(df, catVars)

# Euclidean/Manhattan (built-in)
dist(scale(df[, contVars]), method = "euclidean")
dist(scale(df[, contVars]), method = "manhattan")

# Correlation
as.dist(1 - abs(cor(df[, contVars])))

# Mutual Information
.computeMutualInfoDistance(df, catVars, contVars)

# Mixed
.computeMixedMarkerDistance(df, catVars, contVars)
```

---

## Quick Selection Guide

**Start here:**
```
📊 All categorical? → Chi-squared
📊 Binary only? → Jaccard
📊 All continuous? → Euclidean
📊 Mixed types? → Mixed or Mutual Info
```

**Special cases:**
```
⚠️ Outliers present? → Manhattan or Mutual Info
⚠️ Different scales? → Correlation or Mixed
⚠️ Non-linear relationships? → Mutual Info
⚠️ Rare events? → Jaccard
⚠️ Need statistics? → Chi-squared or Cramér's V
```
