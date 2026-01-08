# Integrating IHC Clustering with Diagnostic Prediction

## Overview

This guide explains how to use the outputs from `ihccluster` analysis to inform diagnostic prediction workflows. The clustering features produce several outputs that can enhance diagnostic classification accuracy and interpretability.

---

## Key Clustering Outputs for Prediction

### 1. **Cluster Assignment**
- Each case is assigned to a cluster based on its IHC expression pattern
- Clusters represent molecularly distinct groups
- Can be used as a categorical predictor variable

### 2. **Silhouette Score**
- Measures how well a case fits within its assigned cluster
- Range: -1 (poor fit) to +1 (excellent fit)
- Low silhouette scores indicate atypical cases that may need molecular confirmation

### 3. **Distance to Cluster Centroid**
- Quantifies how similar a case is to the "typical" case in its cluster
- Smaller distances = more representative of the cluster
- Can identify borderline cases between clusters

### 4. **Marker Performance Metrics**
- Sensitivity, specificity, PPV, NPV for each marker
- Identifies which markers are most discriminative for each diagnosis
- Guides feature selection for prediction models

### 5. **Optimal Antibody Panels**
- Ranked marker combinations with best diagnostic performance
- Provides rule-based classification criteria
- High specificity panels (≥95%) can serve as confirmatory tests

### 6. **Outlier Flags**
- Cases with atypical immunoprofiles (silhouette < 0.25)
- Quality flags: Poor, Very low, Low, Borderline
- Should trigger additional validation in prediction workflows

---

## Integration Approaches

### Approach 1: Rule-Based Prediction (High Specificity)

Use optimal antibody panels as diagnostic rules:

**Workflow:**
1. Run `ihccluster` with `identifyOptimalPanel = TRUE`
2. Extract top-ranked panels (specificity ≥95%, PPV ≥90%)
3. Apply as classification rules: If panel positive → Assign diagnosis

**Advantages:**
- Highly interpretable (clinicians can understand the rule)
- Low false positive rate (high specificity)
- Matches clinical diagnostic workflow

**Limitations:**
- May have lower sensitivity (miss some cases)
- Binary decision (no confidence scores)
- Limited to panel combinations tested

**Example Rule:**
```
IF (EMA = Positive AND CK7 = Positive)
THEN Diagnosis = Synovial_Sarcoma (Confidence: High)
PPV = 100%, Specificity = 100%
```

---

### Approach 2: Distance-Based Prediction (Nearest Centroid)

Use cluster centroids as reference prototypes:

**Workflow:**
1. Run `ihccluster` on training data with known diagnoses
2. Calculate cluster centroids (mean marker expression per cluster)
3. For new cases: Calculate Gower distance to all cluster centroids
4. Assign to nearest cluster → Predict diagnosis

**Advantages:**
- Handles continuous and categorical markers
- Provides confidence scores (inverse of distance)
- Works for cases that don't match panel rules

**Limitations:**
- Less interpretable than rule-based
- Sensitive to outliers in training data
- No direct PPV/sensitivity estimates

**Implementation:**
```r
# Training phase (ihccluster)
cluster_centroids <- aggregate(markers, by = list(cluster = clusters), FUN = mean)
diagnosis_mapping <- table(clusters, diagnosis)  # Cluster → Diagnosis mapping

# Prediction phase (new case)
new_case_distances <- sapply(1:nrow(cluster_centroids), function(i) {
    cluster::daisy(rbind(new_case, cluster_centroids[i, ]), metric = "gower")[1]
})
predicted_cluster <- which.min(new_case_distances)
predicted_diagnosis <- names(which.max(diagnosis_mapping[predicted_cluster, ]))
confidence <- 1 / (1 + new_case_distances[predicted_cluster])
```

---

### Approach 3: Cluster Membership Features (Machine Learning)

Use clustering outputs as features for supervised learning:

**Workflow:**
1. Run `ihccluster` on training data
2. Export cluster-derived features:
   - Cluster assignment (one-hot encoded)
   - Silhouette score
   - Distance to assigned cluster centroid
   - Distance to all cluster centroids
3. Combine with original marker values
4. Train supervised classifier (Random Forest, SVM, Logistic Regression)

**Advantages:**
- Best prediction accuracy
- Captures complex patterns
- Can incorporate outlier information (silhouette)

**Limitations:**
- Requires supervised learning framework
- Less interpretable (black box)
- Risk of overfitting if training set is small

**Feature Set:**
```r
# Original markers
ER, PR, HER2, Ki67, AR, p53, ...

# Cluster-derived features
cluster_id                   # Categorical: Cluster 1/2/3/4
silhouette_score            # Continuous: -1 to +1
dist_to_own_centroid        # Continuous: 0 to 1
dist_to_cluster1_centroid   # Continuous: 0 to 1
dist_to_cluster2_centroid   # Continuous: 0 to 1
...
is_outlier                  # Binary: silhouette < 0.25
```

---

### Approach 4: Hierarchical Prediction (Ensemble)

Combine multiple approaches for robust prediction:

**Workflow:**

**Level 1: Rule-Based Screening (High Specificity)**
- Apply optimal antibody panels
- If panel match found with PPV ≥90% → Report diagnosis with "High Confidence"
- If no match → Proceed to Level 2

**Level 2: Cluster Assignment (Pattern Matching)**
- Assign to nearest cluster based on Gower distance
- Map cluster to most common diagnosis
- Calculate confidence based on cluster purity and silhouette score
- If confidence ≥75% → Report diagnosis with "Moderate Confidence"
- If confidence <75% → Proceed to Level 3

**Level 3: Outlier Flagging (Quality Control)**
- If silhouette < 0.10 → Flag as "Very low confidence - atypical profile"
- Recommend: Molecular testing, clinical correlation, expert review
- Provide differential diagnosis (top 3 cluster/diagnosis combinations)

**Advantages:**
- Combines strengths of all approaches
- Provides confidence levels
- Identifies problematic cases
- Highly interpretable workflow

**Limitations:**
- More complex implementation
- Requires careful threshold calibration

---

## Two-Function Architecture for Jamovi

### Proposed Workflow

**Function 1: `ihccluster` (Training Phase)**
- **Input:** Cases with KNOWN diagnoses
- **Purpose:** Learn clustering patterns, calculate marker performance
- **Output:**
  - Cluster assignments
  - Marker performance metrics
  - Optimal antibody panels
  - Cluster centroids (stored internally)
  - Diagnosis-cluster mapping

**Function 2: `ihcpredict` (Prediction Phase)**
- **Input:** Cases with UNKNOWN diagnoses + Reference to trained model
- **Purpose:** Apply learned patterns to predict diagnoses
- **Output:**
  - Predicted diagnosis per case
  - Confidence level (High/Moderate/Low)
  - Alternative diagnoses (differential)
  - Evidence supporting prediction
  - Flags for atypical cases

### Data Requirements

**Training Dataset (for ihccluster):**
```csv
CaseID,Diagnosis,EMA,CK7,bcl2,CD56,S100,Nestin,NGFR,CD99,Fli1
Train_01,Synovial_Sarcoma,Positive,Positive,Positive,Negative,Negative,Negative,Positive,Positive,Negative
Train_02,MPNST,Negative,Negative,Positive,Negative,Positive,Positive,Positive,Negative,Negative
...
```
- **Required:** Known `Diagnosis` column
- **Minimum:** 10+ cases per diagnosis
- **Recommended:** 20+ cases per diagnosis

**Prediction Dataset (for ihcpredict):**
```csv
CaseID,EMA,CK7,bcl2,CD56,S100,Nestin,NGFR,CD99,Fli1
Query_01,Positive,Negative,Positive,Positive,Positive,Negative,Negative,Positive,Negative
Query_02,Negative,Negative,Negative,Negative,Negative,Positive,Positive,Positive,Negative
...
```
- **Required:** Same marker columns as training data (order doesn't matter)
- **No Diagnosis column** (this will be predicted)
- Can have additional clinical variables (will be ignored for prediction)

### Marker Consistency Validation

**Automatic checks in `ihcpredict`:**
1. ✅ All training markers present in query dataset?
2. ✅ Marker data types match (categorical vs continuous)?
3. ✅ Categorical levels match (e.g., Positive/Negative)?
4. ⚠️ Warning if query dataset has extra markers not in training
5. ❌ Error if query dataset missing required markers

---

## Confidence Score Calculation

### Formula (Hybrid Approach)

```
Confidence = w1 × Panel_Score + w2 × Distance_Score + w3 × Silhouette_Score

Where:
Panel_Score = {
    1.0 if optimal panel match with PPV ≥90%
    0.8 if optimal panel match with PPV 80-89%
    0.5 if optimal panel match with PPV 70-79%
    0.0 otherwise
}

Distance_Score = 1 / (1 + distance_to_nearest_centroid)
    Range: 0 (far) to 1 (very close)

Silhouette_Score = (b - a) / max(a, b)
    Range: -1 (misclassified) to +1 (perfect fit)
    Normalized to 0-1: (Silhouette + 1) / 2

Weights:
w1 = 0.5  # Rule-based evidence (most interpretable)
w2 = 0.3  # Distance-based evidence
w3 = 0.2  # Cluster quality

Confidence_Level = {
    "High"     if Confidence ≥ 0.75
    "Moderate" if Confidence 0.50-0.74
    "Low"      if Confidence 0.25-0.49
    "Very Low" if Confidence < 0.25
}
```

### Interpretation

| Confidence Level | Score Range | Interpretation | Clinical Action |
|------------------|-------------|----------------|-----------------|
| **High** | ≥0.75 | Strong evidence, matches known patterns | Accept diagnosis, proceed with treatment |
| **Moderate** | 0.50-0.74 | Reasonable evidence, some uncertainty | Consider confirmatory testing |
| **Low** | 0.25-0.49 | Weak evidence, atypical features | Recommend molecular testing |
| **Very Low** | <0.25 | Insufficient evidence, outlier profile | Mandatory molecular testing, expert review |

---

## Alternative Diagnoses (Differential)

**Purpose:** Provide top 3 alternative diagnoses when confidence is not 100%

**Calculation:**
1. Calculate confidence scores for all possible diagnoses
2. Rank by confidence
3. Report top 3 with their scores

**Output Table:**
```
Case ID    Primary Diagnosis    Confidence    Alternative 1         Confidence    Alternative 2         Confidence
Query_01   Synovial_Sarcoma     0.85         MPNST                 0.12         Ewing_Sarcoma         0.03
Query_02   MPNST                0.42         Synovial_Sarcoma      0.38         Ewing_Sarcoma         0.20
```

**Clinical Use:**
- Query_01: High confidence (0.85) → Accept Synovial_Sarcoma
- Query_02: Low confidence (0.42) → Consider both MPNST and Synovial_Sarcoma in differential

---

## Evidence Supporting Prediction

**Purpose:** Explain WHY a diagnosis was predicted (interpretability)

**Output Table:**
```
Case ID    Predicted Diagnosis    Evidence
Query_01   Synovial_Sarcoma       • Optimal panel match: EMA + CK7 (PPV=100%, Specificity=100%)
                                  • Assigned to Cluster 1 (95% Synovial_Sarcoma)
                                  • Distance to centroid: 0.12 (excellent fit)
                                  • Silhouette score: 0.68 (good cluster membership)

Query_02   MPNST                  • No optimal panel match found
                                  • Assigned to Cluster 2 (70% MPNST, 30% Ewing_Sarcoma)
                                  • Distance to centroid: 0.45 (moderate fit)
                                  • Silhouette score: 0.18 (borderline - ATYPICAL PROFILE)
                                  • ⚠️ LOW CONFIDENCE - Recommend molecular testing
```

---

## Atypical Case Flagging in Prediction

**Purpose:** Identify query cases with unusual patterns requiring additional validation

**Criteria for Flagging:**
1. Silhouette score < 0.25 (borderline cluster membership)
2. Distance to nearest centroid > 75th percentile of training distances
3. Confidence score < 0.50
4. Marker expression pattern matches no known cluster

**Output Table:**
```
Case ID    Predicted Diagnosis    Quality Flag                Recommendation
Query_02   MPNST                  Low - atypical profile      Molecular testing advised
Query_05   Ewing_Sarcoma          Very low - ambiguous        Strong recommendation for molecular confirmation
Query_12   Synovial_Sarcoma       Poor - misclassified        Review IHC staining quality, repeat markers, mandatory molecular testing
```

---

## Clinical Validation Workflow

### Step 1: Training Phase (Retrospective Cases)
1. Collect 50-100 cases with molecularly confirmed diagnoses
2. Run `ihccluster` with all diagnostic features enabled:
   - ✅ Calculate Marker Performance Metrics
   - ✅ Identify Optimal Antibody Panels
   - ✅ Flag Atypical Cases
3. Review outputs:
   - Marker performance metrics: Validate against published literature
   - Optimal panels: Compare to institutional experience
   - Outlier cases: Check if molecular testing was required
4. Export trained model parameters (stored in jamovi state)

### Step 2: Internal Validation (Held-Out Cases)
1. Randomly split training data 70/30 (train/test)
2. Run `ihccluster` on 70% training set
3. Run `ihcpredict` on 30% test set (treat diagnoses as unknown)
4. Compare predicted diagnoses to true diagnoses
5. Calculate accuracy, sensitivity, specificity per diagnosis
6. Identify prediction failures → Adjust confidence thresholds

### Step 3: Prospective Validation (New Cases)
1. Collect 20-30 new cases with known diagnoses (not in training set)
2. Run `ihcpredict` treating diagnoses as unknown
3. Compare predicted vs actual diagnoses
4. Track confidence calibration: Are "High Confidence" predictions actually correct?
5. Calculate overall accuracy, sensitivity, specificity

### Step 4: Clinical Implementation
1. Deploy for true unknown cases (core biopsies awaiting diagnosis)
2. Always obtain molecular confirmation for:
   - Low confidence predictions (<0.50)
   - Atypical immunoprofiles (silhouette <0.10)
   - Clinically unexpected diagnoses
3. Track outcomes: Update training dataset with confirmed cases
4. Periodic re-training: Re-run `ihccluster` every 6-12 months with expanded dataset

---

## Limitations and Considerations

### 1. **Training Set Size**
- **Minimum:** 10 cases per diagnosis
- **Recommended:** 30+ cases per diagnosis
- **Ideal:** 50+ cases per diagnosis
- Smaller samples → wider confidence intervals, less reliable predictions

### 2. **Training Set Representativeness**
- Training data should match the target population
- If training set is all resection specimens, predictions may be less accurate for core biopsies
- Consider enriching training set with diverse case types

### 3. **Marker Measurement Variability**
- IHC staining is semi-quantitative and lab-dependent
- H-scores and % positivity are subjective
- Ideal: Use same antibodies, protocols, and scoring criteria as training set

### 4. **Novel Diagnoses Not in Training Set**
- Prediction model can only predict diagnoses it has seen
- If query case is a new diagnosis → Will be forced into wrong cluster
- Monitor for very low confidence predictions (potential new entities)

### 5. **Overlapping Immunoprofiles**
- Some diagnoses have similar IHC patterns (e.g., synovial sarcoma vs MPNST with focal cytokeratin)
- High overlap → Lower prediction confidence
- Use differential diagnosis output to guide additional testing

### 6. **Continuous Marker Thresholding**
- Continuous markers (H-score, %) are binarized for optimal panel rules
- Threshold choice affects sensitivity/specificity
- Default: Median split (may not match clinical cutoffs)
- Consider providing custom thresholds (e.g., Ki67 >20% = high)

---

## Future Enhancements

### 1. **Multi-Class Probability Estimates**
Instead of single diagnosis + confidence, provide full probability distribution:
```
Query_01:
  Synovial_Sarcoma: 0.72
  MPNST:            0.18
  Ewing_Sarcoma:    0.10
```

### 2. **Uncertainty Quantification**
- Bootstrap resampling to estimate confidence intervals
- Report: "Predicted diagnosis: Synovial_Sarcoma (95% CI: 0.65-0.82)"

### 3. **External Validation Benchmarking**
- Compare predictions to published diagnostic algorithms
- Report: "Agreement with Olsen 2006 algorithm: κ=0.78 (substantial)"

### 4. **Interactive Prediction Explanation**
- Generate case-specific reports with visualizations:
  - Radar plot showing case vs cluster centroids
  - Marker-by-marker comparison table
  - Similar training cases (nearest neighbors)

### 5. **Model Update Workflow**
- Semi-supervised learning: Add high-confidence predictions to training set
- Active learning: Prioritize molecular testing for informative cases

---

## Summary

### Clustering Outputs Useful for Prediction:
1. ✅ Cluster assignment → Categorical predictor
2. ✅ Silhouette score → Confidence/quality metric
3. ✅ Distance to centroid → Similarity measure
4. ✅ Marker performance metrics → Feature importance
5. ✅ Optimal panels → Rule-based classification
6. ✅ Outlier flags → Quality control

### Recommended Implementation:
- **Two-function architecture:** `ihccluster` (training) + `ihcpredict` (prediction)
- **Hybrid ensemble approach:** Rules + Distance + Cluster membership
- **Confidence levels:** High/Moderate/Low/Very Low
- **Mandatory molecular testing for:** Low confidence (<0.50) + Atypical profiles (silhouette <0.10)

### Clinical Validation Required:
- Internal validation (held-out test set)
- Prospective validation (new cases)
- Periodic model re-training (6-12 months)

---

## References

1. **Olsen SH, et al. (2006)**. Cluster analysis of immunohistochemical profiles in synovial sarcoma, malignant peripheral nerve sheath tumor, and Ewing sarcoma. *Mod Pathol*, 19:659-668.

2. **Matsuoka T, et al. (2011)**. Cluster Analysis of Claudin-1 and -4, E-Cadherin, and β-Catenin Expression in Colorectal Cancers. *J Surg Oncol*, 103:674-686.

3. **Hastie T, Tibshirani R, Friedman J (2009)**. The Elements of Statistical Learning: Data Mining, Inference, and Prediction. 2nd Edition. Springer.

4. **Kaufman L, Rousseeuw PJ (1990)**. Finding Groups in Data: An Introduction to Cluster Analysis. Wiley.