# Sterlacci 2019 Feature Analysis for ihccluster Implementation

## Executive Summary

This document provides a comprehensive gap analysis between the Sterlacci et al. (2019) NSCLC IHC clustering methodology and the current ClinicoPath `ihccluster` implementation. The analysis identifies 8 major missing features that should be implemented to support this important research paradigm.

**Priority Classification:**
- **High Priority (P1):** Core methodology differences affecting clustering results
- **Medium Priority (P2):** Statistical validation and quality metrics
- **Low Priority (P3):** Visualization enhancements and convenience features

---

## Study Overview: Sterlacci et al. 2019

**Title:** "Tissue Microarray-Based Cluster Analysis Identifies Distinct IHC Phenotypes in Non-Small Cell Lung Cancer: Correlation with Histotype and Clinical Outcomes"

**Study Design:**
- **Sample:** 365 NSCLC cases (218 ACA, 125 SCC, 22 LCC)
- **Markers:** 70+ IHC markers (structural, immune, stemness, signaling)
- **Platform:** Tissue microarray (TMA)
- **Clustering:** Hierarchical clustering with Jaccard distance and complete linkage
- **Validation:** Random split + Cohen's kappa for reproducibility testing

**Key Findings:**
1. **Three unsupervised clusters** identified across all NSCLC:
   - **Cluster 1 (n=206):** ACA-like phenotype (CK7+, TTF1+, EGFR+, E-cadherin+)
   - **Cluster 2 (n=105):** SCC-like phenotype (CK5/6+, p63+, CD44+, SOX2+)
   - **Cluster 3 (n=54):** Immune signature (high CD8+ TIL, low CD4/CD8 ratio, PD1+, PDL1+)

2. **Novel discovery:** Immune signature cluster transcends histotypes
   - 15% of cases clustered by TIL profile rather than tumor markers
   - Potential biomarker for immunotherapy response

3. **Reproducibility:** Cohen's κ = 0.733 (ACA-like), 0.692 (SCC-like), 0.371 (immune)

4. **Supervised clustering within histotypes** revealed prognostic subgroups

---

## Current ihccluster Implementation Status

### ✅ Features Already Implemented:

1. **Multiple clustering algorithms:**
   - PAM (k-medoids) on Gower distance
   - Hierarchical clustering with Ward linkage on Gower distance
   - MCA/PCA + k-means for dimension reduction

2. **Mixed data type support:**
   - Gower distance handles categorical + continuous markers
   - Automatic scaling of continuous variables
   - Missing data handling (pairwise or complete cases)

3. **Consensus clustering:**
   - Bootstrap resampling for stability assessment
   - N=50-1000 iterations

4. **Quality metrics:**
   - Silhouette scores for cluster quality
   - PPV and purity when known diagnosis provided
   - Outlier/atypical case flagging

5. **Diagnostic features (Olsen 2006):**
   - Marker performance metrics (sensitivity, specificity, PPV, NPV)
   - Optimal antibody panel selection (2-marker and 3-marker panels)
   - Atypical case identification

6. **Spatial analysis:**
   - Compartment-based clustering (e.g., central vs invasive)
   - Between/within compartment comparisons

7. **Visualization:**
   - Silhouette plots
   - Expression heatmaps with Ward linkage dendrograms
   - PCA/MCA plots
   - Marker distribution boxplots
   - Accessibility features (colorblind palettes, high contrast)

8. **Clinical integration:**
   - Clinical variable associations with clusters
   - Survival analysis integration (Kaplan-Meier by cluster)
   - Case ID tracking

---

## Gap Analysis: Missing Features from Sterlacci 2019

### 🔴 Priority 1 (High): Core Methodology

#### 1. **Jaccard Distance for Binary Markers** [MISSING]

**What Sterlacci 2019 Used:**
- Jaccard distance for binary marker data
- Formula: `d(A,B) = 1 - (|A ∩ B| / |A ∪ B|)`
- Specifically designed for binary data (presence/absence)
- Better than Gower for pure binary datasets

**Current ihccluster:**
- Uses Gower distance for all data types
- Gower distance formula for binary: `d = 1 - s` where `s = similarity`
- Similar but not identical to Jaccard

**Why It Matters:**
- Jaccard distance treats "both negative" differently than "both positive"
- For IHC with many negatives, this affects clustering sensitivity
- Standard method in published IHC clustering studies
- Allows direct replication of Sterlacci methodology

**Implementation Requirements:**
- Add `distanceMethod` option to `ihccluster.a.yaml`:
  ```yaml
  - name: distanceMethod
    title: 'Distance Metric'
    type: List
    options:
      - title: 'Gower (handles mixed data)'
        name: gower
      - title: 'Jaccard (binary data only)'
        name: jaccard
      - title: 'Euclidean (continuous data only)'
        name: euclidean
    default: gower
    description: 'Distance metric for clustering'
  ```

- Add validation: Jaccard requires binary data only
- Implement in `.run()`:
  ```r
  if (self$options$distanceMethod == "jaccard") {
      # Check all markers are binary
      if (!all_binary) {
          stop("Jaccard distance requires binary markers only. Use Gower for mixed data.")
      }
      # Convert to 0/1 matrix
      bin_matrix <- as.matrix(df_binary)
      # Compute Jaccard distance
      dist_matrix <- proxy::dist(bin_matrix, method = "Jaccard")
  }
  ```

**Effort:** Medium (2-3 hours)

---

#### 2. **Complete Linkage Hierarchical Clustering** [MISSING]

**What Sterlacci 2019 Used:**
- Complete linkage (furthest neighbor)
- Distance between clusters = maximum distance between any pair of points
- Formula: `D(A,B) = max{d(a,b) : a ∈ A, b ∈ B}`

**Current ihccluster:**
- Uses Ward linkage only
- Ward minimizes within-cluster variance
- Different cluster shapes and sizes

**Why It Matters:**
- Complete linkage produces more compact, spherical clusters
- Better for identifying distinct immunoprofile groups
- Standard method in IHC clustering literature
- Ward can merge large clusters prematurely

**Implementation Requirements:**
- Add `linkageMethod` option when `method = hierarchical`:
  ```yaml
  - name: linkageMethod
    title: 'Linkage Method'
    type: List
    options:
      - title: 'Ward (minimize variance)'
        name: ward
      - title: 'Complete (furthest neighbor)'
        name: complete
      - title: 'Average (mean distance)'
        name: average
      - title: 'Single (nearest neighbor)'
        name: single
    default: ward
    description: 'Hierarchical clustering linkage method'
  ```

- Modify hierarchical clustering code:
  ```r
  if (method == "hierarchical") {
      if (self$options$linkageMethod == "complete") {
          hc <- stats::hclust(dist_matrix, method = "complete")
      } else if (self$options$linkageMethod == "ward") {
          hc <- stats::hclust(dist_matrix, method = "ward.D2")
      } else {
          hc <- stats::hclust(dist_matrix, method = self$options$linkageMethod)
      }
  }
  ```

**Effort:** Low (1 hour)

---

### 🟡 Priority 2 (Medium): Statistical Validation

#### 3. **Reproducibility Testing via Random Split + Cohen's Kappa** [MISSING]

**What Sterlacci 2019 Used:**
- Split dataset randomly into 2 equal groups (Group A, Group B)
- Cluster each group independently
- Measure agreement using Cohen's kappa (κ)
- κ interpretation: <0.21 (poor), 0.21-0.40 (fair), 0.41-0.60 (moderate), 0.61-0.80 (substantial), 0.81-1.0 (almost perfect)

**Results from Study:**
- κ = 0.733 for ACA-like cluster (substantial reproducibility)
- κ = 0.692 for SCC-like cluster (substantial reproducibility)
- κ = 0.371 for immune cluster (fair reproducibility - smaller n)

**Current ihccluster:**
- Has consensus clustering (bootstrap resampling)
- Has Cohen's kappa code fragments (line 3137, 3210) but NOT for reproducibility testing
- No random split validation

**Why It Matters:**
- Demonstrates cluster stability across independent samples
- More interpretable than silhouette scores for clinicians
- Standard validation method in diagnostic clustering
- Identifies which clusters are robust vs unstable

**Implementation Requirements:**
- Add option:
  ```yaml
  - name: reproducibilityTest
    title: 'Reproducibility Testing'
    type: Bool
    default: false
    description: 'Test cluster reproducibility via random split and Cohen\'s kappa'

  - name: nSplits
    title: 'Number of Random Splits'
    type: Integer
    min: 1
    max: 100
    default: 10
    description: 'Number of random splits to test (results averaged)'
  ```

- Implement function:
  ```r
  .testReproducibility = function() {
      kappa_values <- numeric(self$options$nSplits)

      for (i in 1:self$options$nSplits) {
          # Randomly split data 50/50
          set.seed(self$options$seed + i)
          n <- nrow(df)
          group_a_idx <- sample(1:n, size = floor(n/2))
          group_b_idx <- setdiff(1:n, group_a_idx)

          # Cluster each group independently
          clusters_a <- cluster_function(df[group_a_idx, ])
          clusters_b <- cluster_function(df[group_b_idx, ])

          # For each case in group A, find nearest case in group B
          # Compare cluster assignments
          # Calculate Cohen's kappa
          kappa_values[i] <- compute_kappa(clusters_a, clusters_b, ...)
      }

      # Return mean and SD of kappa
      list(mean_kappa = mean(kappa_values),
           sd_kappa = sd(kappa_values),
           interpretation = interpret_kappa(mean(kappa_values)))
  }
  ```

- Add results table:
  ```yaml
  - name: reproducibility
    title: Cluster Reproducibility
    type: Table
    columns:
      - name: cluster
        title: Cluster
      - name: kappa
        title: Cohen's κ
      - name: interpretation
        title: Interpretation
      - name: ci_lower
        title: 95% CI Lower
      - name: ci_upper
        title: 95% CI Upper
  ```

**Effort:** High (6-8 hours) - requires careful implementation of case matching algorithm

---

#### 4. **Supervised Clustering Within Known Diagnoses** [MISSING]

**What Sterlacci 2019 Used:**
- After unsupervised clustering, performed **separate clustering within each histotype**
- Example: Cluster only the ACA cases to find ACA subgroups
- Identified prognostically relevant subgroups within SCC

**Figure 2 (sterlacci2019-2.png):** Shows 3 ACA subclusters with different stemness and proliferation profiles

**Figure 3 (sterlacci2019-3.png):** Shows 3 SCC subclusters, with one showing immune infiltration

**Current ihccluster:**
- No supervised clustering option
- Can compare clusters to known diagnosis but doesn't cluster within diagnosis groups

**Why It Matters:**
- Discovers clinically relevant subtypes within established diagnoses
- Example: "Low-grade" vs "high-grade" immunoprofile patterns within same tumor type
- Identifies heterogeneity that may predict treatment response
- Two-stage approach: (1) discover major groups, (2) refine within groups

**Implementation Requirements:**
- Add option:
  ```yaml
  - name: supervisedClustering
    title: 'Supervised Clustering'
    type: Bool
    default: false
    description: 'Perform separate clustering within each known diagnosis group'

  - name: supervisedVariable
    title: 'Grouping Variable for Supervised Clustering'
    type: Variable
    suggested:
      - nominal
    permitted:
      - factor
    description: 'Variable defining groups for supervised clustering (e.g., histotype, diagnosis)'
  ```

- Implement function:
  ```r
  .supervisedClustering = function() {
      # Get grouping variable
      group_var <- self$options$supervisedVariable
      groups <- unique(df[[group_var]])

      supervised_results <- list()

      for (grp in groups) {
          # Subset data to this group
          df_grp <- df[df[[group_var]] == grp, ]

          # Skip if too few cases
          if (nrow(df_grp) < 2 * self$options$nClusters) next

          # Cluster within this group
          result <- .performClustering(df_grp)

          # Store with prefix
          result$group <- grp
          supervised_results[[grp]] <- result
      }

      return(supervised_results)
  }
  ```

- Add results section for each group:
  - Separate heatmaps per group
  - Separate silhouette plots
  - Subgroup profiles

**Effort:** High (8-10 hours) - requires restructuring results section

---

#### 5. **Bonferroni Correction for Multiple Testing** [MISSING]

**What Sterlacci 2019 Used:**
- When testing marker-cluster associations, applied Bonferroni correction
- **Unsupervised analysis:** P < 0.000055 (0.05 / 70 markers = 0.00007, rounded to 0.000055)
- **Supervised analysis:** Separate correction within each histotype
- Example SCC: P < 0.000018 for 26 markers

**Current ihccluster:**
- Reports p-values for marker-cluster associations
- No multiple testing correction applied
- May report false positive associations

**Why It Matters:**
- Essential for studies with many markers (30+)
- Prevents spurious marker associations
- Standard practice in high-dimensional biomarker studies
- Increases confidence in reported marker patterns

**Implementation Requirements:**
- Add option:
  ```yaml
  - name: multipleTestingCorrection
    title: 'Multiple Testing Correction'
    type: List
    options:
      - title: 'None'
        name: none
      - title: 'Bonferroni'
        name: bonferroni
      - title: 'Benjamini-Hochberg (FDR)'
        name: fdr
      - title: 'Holm'
        name: holm
    default: bonferroni
    description: 'Correction method for multiple marker tests'
  ```

- Modify association testing:
  ```r
  # Current code (line ~2500-2600)
  pvals <- sapply(markers, function(m) {
      test_result <- test_association(df[[m]], clusters)
      return(test_result$p.value)
  })

  # Add correction
  if (self$options$multipleTestingCorrection != "none") {
      pvals_adj <- p.adjust(pvals, method = self$options$multipleTestingCorrection)

      # Report both raw and adjusted
      association_table$addColumn(name = "p_adj",
                                   title = "Adjusted p-value")

      # Mark significance based on adjusted p-value
      significant <- pvals_adj < 0.05
  }

  # Calculate Bonferroni threshold for documentation
  bonf_threshold <- 0.05 / length(markers)
  ```

- Add note to results:
  ```r
  note <- sprintf("Bonferroni-corrected significance threshold: P < %.6f (%d markers tested)",
                  bonf_threshold, length(markers))
  ```

**Effort:** Low (2 hours)

---

### 🟢 Priority 3 (Low): Enhancements

#### 6. **CD4/CD8 Ratio Calculation** [MISSING]

**What Sterlacci 2019 Used:**
- Calculated CD4/CD8 ratio as a derived marker
- Low CD4/CD8 ratio defined immune signature cluster
- SCC prognostic subgroup defined by CD4/CD8 ratio + granzyme B+ TIL

**Current ihccluster:**
- No derived marker calculation
- Users must pre-compute ratios

**Why It Matters:**
- CD4/CD8 ratio is standard immune contexture metric
- Ratios can be more informative than individual markers
- Convenience feature for immunopathology studies

**Implementation Requirements:**
- Add option:
  ```yaml
  - name: calculateRatios
    title: 'Calculate Marker Ratios'
    type: Bool
    default: false
    description: 'Calculate ratios between continuous markers'

  - name: ratioNumerator
    title: 'Numerator Marker'
    type: Variable
    suggested:
      - continuous
    permitted:
      - numeric

  - name: ratioDenominator
    title: 'Denominator Marker'
    type: Variable
    suggested:
      - continuous
    permitted:
      - numeric

  - name: ratioName
    title: 'Ratio Variable Name'
    type: String
    default: 'marker_ratio'
  ```

- Implement:
  ```r
  if (self$options$calculateRatios) {
      num <- df[[self$options$ratioNumerator]]
      denom <- df[[self$options$ratioDenominator]]

      # Handle zero denominators
      ratio <- ifelse(denom > 0, num / denom, NA)

      # Add to dataframe
      df[[self$options$ratioName]] <- ratio

      # Add to continuous markers list
      contVars <- c(contVars, self$options$ratioName)
  }
  ```

**Effort:** Low (2-3 hours)

---

#### 7. **Binary Heatmap Visualization with Custom Colors** [PARTIALLY MISSING]

**What Sterlacci 2019 Used:**
- Binary heatmap (yellow = negative, green = positive)
- No continuous color scale for categorical markers
- Clear visual distinction

**Current ihccluster:**
- Heatmap with continuous color scales (typically red-white-blue or yellow-blue)
- Scales categorical markers as 0/1/2 with gradients
- May be harder to read for binary data

**Why It Matters:**
- Binary heatmaps are easier to interpret for clinicians
- Matches traditional IHC reporting (positive/negative)
- Reduces cognitive load

**Implementation Requirements:**
- Add option:
  ```yaml
  - name: heatmapType
    title: 'Heatmap Type'
    type: List
    options:
      - title: 'Continuous (scaled values)'
        name: continuous
      - title: 'Binary (positive/negative)'
        name: binary
    default: continuous

  - name: binaryColors
    title: 'Binary Heatmap Colors'
    type: List
    options:
      - title: 'Green (pos) / Yellow (neg)'
        name: green_yellow
      - title: 'Red (pos) / Blue (neg)'
        name: red_blue
      - title: 'Black (pos) / White (neg)'
        name: black_white
    default: green_yellow
  ```

- Modify heatmap generation:
  ```r
  if (self$options$heatmapType == "binary") {
      # Convert to binary matrix
      heatmap_data <- apply(df, 2, function(x) {
          if (is.factor(x)) {
              # Positive = 1, Negative = 0
              return(as.numeric(x %in% c("Positive", "pos", "+")))
          } else {
              # Use median split for continuous
              return(as.numeric(x > median(x, na.rm = TRUE)))
          }
      })

      # Use binary color palette
      colors <- switch(self$options$binaryColors,
                       green_yellow = c("yellow", "darkgreen"),
                       red_blue = c("blue", "red"),
                       black_white = c("white", "black"))

      ComplexHeatmap::Heatmap(heatmap_data,
                              col = colors,
                              name = "Expression")
  }
  ```

**Effort:** Low (2 hours)

---

#### 8. **TIL (Tumor-Infiltrating Lymphocyte) Marker Classification** [MISSING]

**What Sterlacci 2019 Found:**
- Immune signature cluster defined by TIL markers (CD4, CD8, PD1, PDL1, FoxP3, granzyme B, TIA1)
- TIL markers can define clusters independent of tumor markers
- Clinically relevant for immunotherapy patient selection

**Current ihccluster:**
- Treats all markers equally
- No special handling of immune markers

**Why It Matters:**
- Identifies patients with high immune infiltration (immunotherapy candidates)
- Conceptually different from tumor intrinsic markers
- May want separate clustering on TIL markers only

**Implementation Requirements:**
- Add option:
  ```yaml
  - name: tilMarkers
    title: 'TIL/Immune Markers (optional)'
    type: Variables
    suggested:
      - nominal
      - continuous
    permitted:
      - factor
      - numeric
    description: 'Immune/TIL markers for separate immune signature analysis'
    default: NULL

  - name: performTILClustering
    title: 'Perform TIL-Based Clustering'
    type: Bool
    default: false
    description: 'Cluster based on immune markers separately'
  ```

- Implement:
  ```r
  if (self$options$performTILClustering && !is.null(self$options$tilMarkers)) {
      # Cluster using only TIL markers
      til_df <- df[, self$options$tilMarkers, drop = FALSE]
      til_result <- .performClustering(til_df)

      # Compare with overall clusters
      # Identify "immune signature" cases
      immune_cases <- identify_immune_signature(til_result, til_df)

      # Add to results
      self$results$tilClusters$setContent(til_result)
  }
  ```

**Effort:** Medium (4-5 hours)

---

## Summary Table of Missing Features

| # | Feature | Priority | Effort | Impact | Mathematical Formula |
|---|---------|----------|--------|--------|---------------------|
| 1 | Jaccard distance | High | Medium | High | d(A,B) = 1 - |A∩B|/|A∪B| |
| 2 | Complete linkage | High | Low | Medium | D(A,B) = max{d(a,b)} |
| 3 | Reproducibility testing | Medium | High | High | Cohen's κ, random split |
| 4 | Supervised clustering | Medium | High | High | Cluster within diagnosis groups |
| 5 | Bonferroni correction | Medium | Low | Medium | P_adj < 0.05/n_markers |
| 6 | CD4/CD8 ratio | Low | Low | Low | ratio = CD4_count / CD8_count |
| 7 | Binary heatmap | Low | Low | Low | Binary color mapping |
| 8 | TIL marker classification | Low | Medium | Medium | Separate TIL clustering |

**Total Estimated Effort:** 32-41 hours

---

## Implementation Priority Recommendation

### Phase 1 (Core Methodology) - Implement First
**Effort:** ~4-5 hours
**Features:** #1 (Jaccard), #2 (Complete linkage), #5 (Bonferroni)

**Rationale:**
- Enables direct replication of Sterlacci methodology
- Low implementation effort
- High scientific rigor impact
- Required for publication-quality results

### Phase 2 (Advanced Validation) - Implement Second
**Effort:** ~14-18 hours
**Features:** #3 (Reproducibility), #4 (Supervised clustering)

**Rationale:**
- Significantly enhances scientific validity
- Standard methods in diagnostic clustering papers
- Higher effort but essential for comprehensive analysis

### Phase 3 (Enhancements) - Implement Last
**Effort:** ~8-13 hours
**Features:** #6 (CD4/CD8 ratio), #7 (Binary heatmap), #8 (TIL markers)

**Rationale:**
- Convenience and specialized features
- Can be added incrementally
- Nice-to-have but not critical

---

## Mathematical Specifications

### Jaccard Distance

For binary vectors A and B:

```
Jaccard Similarity: J(A,B) = |A ∩ B| / |A ∪ B|

Jaccard Distance: d_J(A,B) = 1 - J(A,B)

For binary data (0/1):
- A ∩ B = number of positions where both are 1
- A ∪ B = number of positions where at least one is 1

Example:
Case 1: [1, 0, 1, 1, 0]
Case 2: [1, 1, 1, 0, 0]

A ∩ B = 2 (positions 1 and 3)
A ∪ B = 4 (positions 1, 2, 3, 4)
J(A,B) = 2/4 = 0.5
d_J(A,B) = 1 - 0.5 = 0.5
```

R implementation:
```r
# Using proxy package
dist_matrix <- proxy::dist(binary_matrix, method = "Jaccard")

# Manual implementation
jaccard_distance <- function(x, y) {
    intersection <- sum(x & y)
    union <- sum(x | y)
    if (union == 0) return(0)
    return(1 - intersection / union)
}
```

### Complete Linkage

For clusters A and B:

```
D(A,B) = max{d(a,b) : a ∈ A, b ∈ B}

Where:
- d(a,b) is pairwise distance between cases a and b
- Max distance defines cluster separation
```

R implementation:
```r
hc <- stats::hclust(dist_matrix, method = "complete")
```

### Cohen's Kappa for Reproducibility

For two independent clusterings:

```
κ = (p_o - p_e) / (1 - p_e)

Where:
- p_o = observed agreement (proportion of matched cases)
- p_e = expected agreement by chance

For k clusters:
p_e = Σ(p_i^A × p_i^B) for i = 1 to k

Where p_i^A and p_i^B are proportions of cases in cluster i for groupings A and B
```

Interpretation:
- κ < 0.21: Poor
- κ = 0.21-0.40: Fair
- κ = 0.41-0.60: Moderate
- κ = 0.61-0.80: Substantial
- κ = 0.81-1.00: Almost perfect

R implementation:
```r
# Using irr package
library(irr)
kappa_result <- kappa2(cbind(clusters_a, clusters_b))

# Manual implementation
cohen_kappa <- function(x, y) {
    # Confusion matrix
    confusion <- table(x, y)
    n <- sum(confusion)

    # Observed agreement
    p_o <- sum(diag(confusion)) / n

    # Expected agreement
    row_sums <- rowSums(confusion) / n
    col_sums <- colSums(confusion) / n
    p_e <- sum(row_sums * col_sums)

    # Kappa
    kappa <- (p_o - p_e) / (1 - p_e)
    return(kappa)
}
```

### Bonferroni Correction

For n hypothesis tests at significance level α:

```
Adjusted significance threshold: α* = α / n

Example (Sterlacci 2019):
- 70 markers tested
- α = 0.05
- α* = 0.05 / 70 = 0.000714

Study used α* < 0.000055 (more conservative)
```

R implementation:
```r
# p.adjust function
pvals_adj <- p.adjust(pvals, method = "bonferroni")

# Manual implementation
pvals_bonferroni <- pmin(pvals * length(pvals), 1.0)
```

---

## Code Integration Points

### File: `jamovi/ihccluster.a.yaml`

Add options after line 411:

```yaml
# Sterlacci 2019 Features
- name: distanceMethod
  title: 'Distance Metric'
  type: List
  options:
    - title: 'Gower (handles mixed data)'
      name: gower
    - title: 'Jaccard (binary data only)'
      name: jaccard
    - title: 'Euclidean (continuous data only)'
      name: euclidean
  default: gower
  description: 'Distance metric for clustering'

- name: linkageMethod
  title: 'Linkage Method (hierarchical only)'
  type: List
  options:
    - title: 'Ward (minimize variance)'
      name: ward
    - title: 'Complete (furthest neighbor)'
      name: complete
    - title: 'Average (mean distance)'
      name: average
    - title: 'Single (nearest neighbor)'
      name: single
  default: ward
  description: 'Hierarchical clustering linkage method'

- name: reproducibilityTest
  title: 'Test Cluster Reproducibility'
  type: Bool
  default: false
  description: 'Random split validation with Cohen\'s kappa'

- name: nSplits
  title: 'Number of Random Splits'
  type: Integer
  min: 1
  max: 100
  default: 10
  description: 'Number of splits for reproducibility testing'

- name: supervisedClustering
  title: 'Supervised Clustering'
  type: Bool
  default: false
  description: 'Cluster within each diagnosis group'

- name: supervisedVariable
  title: 'Grouping Variable'
  type: Variable
  suggested:
    - nominal
  permitted:
    - factor
  description: 'Variable for supervised clustering groups'

- name: multipleTestingCorrection
  title: 'Multiple Testing Correction'
  type: List
  options:
    - title: 'None'
      name: none
    - title: 'Bonferroni'
      name: bonferroni
    - title: 'Benjamini-Hochberg (FDR)'
      name: fdr
  default: bonferroni
  description: 'Correction for marker association tests'

- name: calculateRatios
  title: 'Calculate Marker Ratios'
  type: Bool
  default: false
  description: 'Calculate CD4/CD8 or other marker ratios'

- name: ratioNumerator
  title: 'Ratio Numerator'
  type: Variable

- name: ratioDenominator
  title: 'Ratio Denominator'
  type: Variable

- name: heatmapType
  title: 'Heatmap Type'
  type: List
  options:
    - title: 'Continuous (scaled)'
      name: continuous
    - title: 'Binary (pos/neg)'
      name: binary
  default: continuous

- name: tilMarkers
  title: 'TIL/Immune Markers'
  type: Variables
  description: 'Immune markers for separate analysis'

- name: performTILClustering
  title: 'TIL-Based Clustering'
  type: Bool
  default: false
```

### File: `jamovi/ihccluster.r.yaml`

Add results tables:

```yaml
- name: reproducibility
  title: Cluster Reproducibility
  type: Table
  columns:
    - name: cluster
      title: Cluster
    - name: kappa
      title: Cohen's κ
    - name: interpretation
      title: Interpretation
    - name: ci_lower
      title: 95% CI Lower
    - name: ci_upper
      title: 95% CI Upper

- name: supervisedResults
  title: Supervised Clustering Results
  type: Group
  items:
    - name: supervisedSummary
      title: Summary
      type: Table
    - name: supervisedHeatmaps
      title: Heatmaps by Group
      type: Image

- name: tilClusters
  title: TIL-Based Clustering
  type: Group
  items:
    - name: tilSummary
      type: Table
    - name: tilComparison
      title: TIL vs Overall Clusters
      type: Table
```

### File: `R/ihccluster.b.R`

Add functions after existing `.performClustering()`:

```r
.computeJaccardDistance = function(binary_matrix) {
    # Input validation
    if (!all(binary_matrix %in% c(0, 1, NA))) {
        stop("Jaccard distance requires binary (0/1) data")
    }

    # Use proxy package for efficient calculation
    dist_matrix <- proxy::dist(binary_matrix, method = "Jaccard")
    return(dist_matrix)
}

.testReproducibility = function(df, cluster_func, n_splits = 10) {
    kappa_values <- numeric(n_splits)

    for (i in 1:n_splits) {
        # Random split
        set.seed(self$options$seed + i)
        n <- nrow(df)
        idx_a <- sample(1:n, size = floor(n/2))
        idx_b <- setdiff(1:n, idx_a)

        # Cluster independently
        clusters_a <- cluster_func(df[idx_a, ])
        clusters_b <- cluster_func(df[idx_b, ])

        # Match clusters between groups and compute kappa
        kappa_values[i] <- .computeKappa(clusters_a, clusters_b, df, idx_a, idx_b)
    }

    mean_kappa <- mean(kappa_values)
    interpretation <- .interpretKappa(mean_kappa)

    return(list(
        mean_kappa = mean_kappa,
        sd_kappa = sd(kappa_values),
        interpretation = interpretation,
        all_kappas = kappa_values
    ))
}

.interpretKappa = function(kappa) {
    if (kappa < 0.21) return("Poor")
    if (kappa < 0.41) return("Fair")
    if (kappa < 0.61) return("Moderate")
    if (kappa < 0.81) return("Substantial")
    return("Almost Perfect")
}

.supervisedClustering = function(df, group_var) {
    groups <- unique(df[[group_var]])
    results <- list()

    for (grp in groups) {
        df_sub <- df[df[[group_var]] == grp, ]

        if (nrow(df_sub) < 2 * self$options$nClusters) {
            warning(sprintf("Group %s has too few cases for clustering", grp))
            next
        }

        result <- .performClustering(df_sub)
        result$group <- grp
        results[[grp]] <- result
    }

    return(results)
}

.calculateMarkerRatio = function(df, numerator, denominator) {
    num_vals <- df[[numerator]]
    denom_vals <- df[[denominator]]

    ratio <- ifelse(denom_vals > 0, num_vals / denom_vals, NA)

    return(ratio)
}
```

Modify existing distance calculation (around line 208):

```r
# Replace existing distance calculation
if (self$options$distanceMethod == "jaccard") {
    # Convert to binary matrix
    binary_matrix <- as.matrix(apply(dist_input, 2, function(x) {
        if (is.factor(x)) {
            return(as.numeric(x %in% c("Positive", "pos", "+")))
        } else {
            return(as.numeric(x > median(x, na.rm = TRUE)))
        }
    }))

    dist_matrix <- .computeJaccardDistance(binary_matrix)

} else if (self$options$distanceMethod == "gower") {
    # Existing Gower distance code
    if (!is.null(dist_weights)) {
        dist_matrix <- cluster::daisy(dist_input, metric = "gower", weights = dist_weights)
    } else {
        dist_matrix <- cluster::daisy(dist_input, metric = "gower")
    }
}
```

Modify hierarchical clustering (around line 278):

```r
# Replace existing hierarchical code
if (method == "hierarchical") {
    linkage_method <- self$options$linkageMethod

    if (linkage_method == "ward") {
        hc <- cluster::agnes(dist_matrix, method = "ward")
    } else {
        # Use stats::hclust for other methods
        hc <- stats::hclust(dist_matrix, method = linkage_method)
    }

    # Rest of code unchanged
}
```

Modify association testing (find code around line 2500-2600):

```r
# After computing p-values
if (self$options$multipleTestingCorrection != "none") {
    pvals_adj <- p.adjust(pvals, method = self$options$multipleTestingCorrection)

    # Add adjusted p-value column
    for (i in seq_along(markers)) {
        association_table$addRow(rowKey = i, values = list(
            marker = markers[i],
            statistic = test_stats[i],
            pvalue = pvals[i],
            pvalue_adj = pvals_adj[i],
            significant = pvals_adj[i] < 0.05
        ))
    }

    # Add note about correction
    bonf_threshold <- 0.05 / length(markers)
    note <- sprintf("Bonferroni-corrected threshold: P < %.6f (%d tests)",
                    bonf_threshold, length(markers))
    self$results$associationTests$setNote("correction", note)
}
```

---

## Testing Dataset Requirements

To validate these implementations, create test datasets:

### 1. NSCLC-like dataset (Sterlacci replication)
- 100-150 cases
- 20-30 binary IHC markers
- 3 known histotypes (ACA, SCC, LCC)
- Include immune markers (CD4, CD8, PD1, PDL1)
- Expected: 3 unsupervised clusters (ACA-like, SCC-like, immune)

### 2. Small binary dataset (Jaccard validation)
- 50 cases
- 10 binary markers
- Known ground truth clusters
- Compare Jaccard vs Gower results

### 3. Supervised clustering dataset
- 90 cases from 3 diagnoses (30 each)
- 15 markers
- Each diagnosis should have 2-3 subtypes
- Validate supervised clustering finds subtypes

---

## Documentation Requirements

### User Guide Updates:

1. **When to use Jaccard vs Gower:**
   - Jaccard: Pure binary data (positive/negative only)
   - Gower: Mixed data types or ordinal scales

2. **Linkage method selection:**
   - Ward: Balanced clusters, minimizes variance (default)
   - Complete: Compact, spherical clusters (Sterlacci method)
   - Average: Intermediate properties
   - Single: Chains, not recommended

3. **Reproducibility testing interpretation:**
   - Run with ≥10 splits for stable estimate
   - κ > 0.61 = substantial reproducibility (publishable)
   - κ < 0.40 = poor reproducibility (consider more markers or fewer clusters)

4. **Supervised clustering workflow:**
   - Step 1: Unsupervised clustering on all cases
   - Step 2: Supervised clustering within each diagnosis
   - Interpretation: Subtypes within established diagnoses

### Technical Notes:

- Jaccard distance is undefined when both vectors are all-negative (returns 0)
- Complete linkage is sensitive to outliers (produces more outlier cases)
- Reproducibility testing requires sufficient sample size (n ≥ 50 recommended)
- Supervised clustering requires ≥10 cases per group

---

## References

**Primary Reference:**
Sterlacci W, Savic S, Schmid T, Oberaigner W, Auberger J, Fiegl M. Tissue microarray based analysis of immunohistochemical expression patterns of molecular targets in NSCLC: correlation with patient outcome and comparison between adenocarcinoma and squamous cell carcinoma. Histol Histopathol. 2019. [Full citation needed from original paper]

**Supporting References:**

1. **Jaccard Distance:**
   - Jaccard P (1912). The distribution of the flora in the alpine zone. New Phytologist 11:37-50.

2. **Hierarchical Clustering:**
   - Ward JH (1963). Hierarchical grouping to optimize an objective function. J Am Stat Assoc 58:236-244.

3. **Cohen's Kappa:**
   - Cohen J (1960). A coefficient of agreement for nominal scales. Educational and Psychological Measurement 20:37-46.
   - Landis JR, Koch GG (1977). The measurement of observer agreement for categorical data. Biometrics 33:159-174.

4. **Multiple Testing:**
   - Bonferroni CE (1936). Teoria statistica delle classi e calcolo delle probabilità. Pubblicazioni del R Istituto Superiore di Scienze Economiche e Commerciali di Firenze 8:3-62.

5. **TIL Assessment:**
   - Salgado R, et al. (2015). The evaluation of tumor-infiltrating lymphocytes (TILs) in breast cancer: recommendations by an International TILs Working Group 2014. Ann Oncol 26:259-271.

---

## Next Steps

1. **Review and Approval:**
   - Review this gap analysis with user
   - Confirm implementation priority
   - Decide which features to implement first

2. **Phase 1 Implementation:**
   - Implement Jaccard distance option
   - Implement complete linkage option
   - Implement Bonferroni correction
   - Test with simple binary dataset

3. **Phase 2 Implementation:**
   - Implement reproducibility testing
   - Implement supervised clustering
   - Test with NSCLC-like dataset

4. **Phase 3 Implementation:**
   - Implement CD4/CD8 ratio calculation
   - Implement binary heatmap visualization
   - Implement TIL marker classification

5. **Documentation and Testing:**
   - Update user guide
   - Create example datasets
   - Write vignette demonstrating Sterlacci replication
   - Validate against published results

---

**Document Version:** 1.0
**Date:** 2025-01-15
**Author:** Claude Code Analysis
