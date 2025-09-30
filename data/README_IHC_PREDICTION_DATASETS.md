# IHC Prediction Datasets - User Guide

## Overview

This folder contains comprehensive, realistic datasets for testing the IHC diagnostic prediction workflow (`ihccluster` → `ihcpredict`).

---

## Dataset Files

### 1. Training Dataset
**File:** `sarcoma_ihc_training_realistic.csv`

**Purpose:** Train the diagnostic model

**Contents:**
- **75 cases** with molecularly confirmed diagnoses
- **3 diagnostic categories:**
  - Synovial Sarcoma (n=28)
  - Malignant Peripheral Nerve Sheath Tumor / MPNST (n=25)
  - Ewing Sarcoma (n=22)

**IHC Markers (9 categorical + 1 continuous):**
- EMA (Epithelial Membrane Antigen)
- CK7 (Cytokeratin 7)
- bcl-2
- CD56
- S100
- Nestin
- NGFR (Nerve Growth Factor Receptor)
- CD99
- Fli-1
- Ki67_Percent (continuous, 0-100%)

**Special Features:**
- Realistic marker distributions based on Olsen et al. (2006)
- Includes **7 atypical cases** (outliers) representing biological variability:
  - 3 CK7-negative Synovial Sarcomas
  - 2 EMA-positive MPNSTs (aberrant expression)
  - 2 Fli1-negative Ewing Sarcomas

**Use with:** `ihccluster` analysis to build diagnostic model

---

### 2. Query Dataset (For Prediction)
**File:** `sarcoma_ihc_query_realistic.csv`

**Purpose:** Test diagnostic prediction

**Contents:**
- **15 query cases** with unknown diagnoses
- **Same IHC markers** as training dataset
- **NO Diagnosis column** (this is what we predict)

**Case Composition:**
- **8 typical cases** (expected high confidence):
  - 3 classic Synovial Sarcomas
  - 2 classic MPNSTs
  - 3 classic Ewing Sarcomas

- **3 atypical variants** (expected moderate confidence):
  - 1 CK7-negative Synovial Sarcoma
  - 1 Fli1-negative Ewing Sarcoma
  - 1 EMA-positive MPNST

- **4 ambiguous cases** (expected low/very low confidence):
  - Borderline immunoprofiles
  - Mixed features
  - Cases requiring molecular confirmation

**Use with:** `ihcpredict` analysis to generate predictions

---

### 3. Query Dataset with Validation Answers
**File:** `sarcoma_ihc_query_with_validation.csv`

**Purpose:** Validation and teaching

**Contents:**
- Same as query dataset PLUS `True_Diagnosis_For_Validation` column
- Allows you to compare predicted vs. actual diagnoses
- **DO NOT use this file for prediction** - it contains the answers!

**Use for:**
- Validating prediction accuracy
- Teaching/demonstration purposes
- Troubleshooting analysis
- Understanding confidence score interpretation

---

## Quick Start Guide

### Step 1: Train the Model with ihccluster

1. Open jamovi
2. Load `sarcoma_ihc_training_realistic.csv`
3. Run **Analyses → OncoPath → IHC Analysis → IHC Clustering Analysis**
4. Settings:
   - Categorical IHC Markers: EMA, CK7, bcl2, CD56, S100, Nestin, NGFR, CD99, Fli1
   - Continuous IHC Markers: Ki67_Percent
   - Known Diagnosis: Diagnosis
   - ✅ Calculate Marker Performance Metrics
   - ✅ Identify Optimal Antibody Panels (Both 2 and 3-marker panels)
   - ✅ Flag Atypical Cases
5. Review outputs:
   - Optimal panels for each diagnosis
   - Marker performance metrics (sensitivity, specificity, PPV, NPV)
   - Outlier cases flagged
6. Note the **file path** to training CSV (you'll need it for prediction)

### Step 2: Predict Unknown Cases with ihcpredict

1. In jamovi, load `sarcoma_ihc_query_realistic.csv`
2. Run **Analyses → OncoPath → IHC Analysis → IHC Diagnostic Prediction**
3. Settings:
   - Model Source: `Load training data from CSV file`
   - Training Data File: `/full/path/to/sarcoma_ihc_training_realistic.csv`
   - Diagnosis Column: `Diagnosis`
   - Categorical IHC Markers: EMA, CK7, bcl2, CD56, S100, Nestin, NGFR, CD99, Fli1
   - Continuous IHC Markers: Ki67_Percent
   - Case ID: CaseID
   - Prediction Method: `Hybrid (rules + distance + cluster quality)`
   - ✅ Show Alternative Diagnoses (3 alternatives)
   - ✅ Show Prediction Evidence
   - ✅ Flag Low Confidence Cases
   - ✅ Show Marker Comparison
4. Review predictions and confidence levels

### Step 3: Validate Results (Optional)

1. Open `sarcoma_ihc_query_with_validation.csv` in Excel
2. Compare `Predicted_Diagnosis` from jamovi with `True_Diagnosis_For_Validation`
3. Check if low confidence cases were correctly flagged

---

## Expected Results

### Optimal Diagnostic Panels (from ihccluster)

Based on training data, you should see these top-ranked panels:

1. **Synovial Sarcoma:** EMA + CK7
   - Specificity: 100%
   - Sensitivity: ~52%
   - PPV: 100%

2. **MPNST:** S100 + Nestin
   - Specificity: ~96%
   - Sensitivity: ~68%
   - PPV: ~94%

3. **Ewing Sarcoma:** CD99 + Fli1
   - Specificity: ~92%
   - Sensitivity: ~73%
   - PPV: ~88%

### Query Case Predictions (from ihcpredict)

Expected confidence distribution:

| Case ID | True Diagnosis | Expected Confidence | Reasoning |
|---------|----------------|---------------------|-----------|
| Query_001-003 | Synovial_Sarcoma | High (≥0.75) | Typical immunoprofile |
| Query_004-005 | MPNST | High (≥0.75) | Typical immunoprofile |
| Query_006-008 | Ewing_Sarcoma | High (≥0.75) | Typical immunoprofile |
| Query_009 | Synovial_Sarcoma | Moderate (0.50-0.74) | CK7 negative variant |
| Query_010 | MPNST | Low (0.25-0.49) | Aberrant EMA positivity |
| Query_011 | Ewing_Sarcoma | Moderate (0.50-0.74) | Fli1 negative variant |
| Query_012 | MPNST | Very Low (<0.25) | Highly atypical profile |
| Query_013 | Synovial_Sarcoma | High (≥0.75) | Optimal panel match (EMA+/CK7+) |
| Query_014 | MPNST | High (≥0.75) | Optimal panel match (S100+/Nestin+) |
| Query_015 | Ewing_Sarcoma | High (≥0.75) | Optimal panel match (CD99+/Fli1+) |

**Overall Expected Accuracy:** ~87% (13/15 correct)

**High Confidence Accuracy:** 100% (8/8 correct)

**Cases Requiring Molecular Testing:** Query_010, Query_012 (flagged as low/very low confidence)

---

## Teaching Points

### Case Studies for Discussion

#### Case Query_013 (High Confidence Synovial Sarcoma)

**IHC Profile:**
- EMA: Positive
- CK7: Positive ← **Optimal panel match!**
- bcl-2: Positive
- CD99: Positive
- All others: Appropriate pattern

**Prediction:** Synovial Sarcoma, confidence 0.88-0.92

**Teaching Point:** When EMA and CK7 are both positive, Synovial Sarcoma has 100% PPV in training data. Algorithm correctly identifies this high-specificity pattern.

**Clinical Action:** Accept diagnosis, proceed with staging. Molecular confirmation (SYT-SSX fusion) recommended but not mandatory.

---

#### Case Query_009 (Moderate Confidence - Atypical Synovial Sarcoma)

**IHC Profile:**
- EMA: Positive
- CK7: Negative ← **Atypical! (Expected positive)**
- bcl-2: Positive
- Nestin: Positive ← **Unusual for Synovial Sarcoma**
- CD56: Positive

**Prediction:** Synovial Sarcoma, confidence 0.45-0.60

**Teaching Point:** Algorithm correctly flags this as atypical due to CK7 negativity and aberrant nestin expression. Differential includes MPNST.

**Clinical Action:** Molecular testing strongly recommended to distinguish Synovial Sarcoma (SYT-SSX fusion) from MPNST (H3K27me3 loss).

---

#### Case Query_010 (Low Confidence - Diagnostic Dilemma)

**IHC Profile:**
- EMA: Positive ← **Unusual for MPNST**
- S100: Positive
- Nestin: Positive
- NGFR: Positive

**Prediction:** MPNST or Synovial Sarcoma (close scores)

**Teaching Point:** True MPNST with aberrant EMA expression. Algorithm correctly identifies this as low confidence because EMA positivity is more typical of Synovial Sarcoma. This represents a real diagnostic challenge.

**Clinical Action:** Mandatory molecular testing. Discuss at tumor board.

---

#### Case Query_012 (Very Low Confidence - Outlier)

**IHC Profile:**
- Mixed/ambiguous pattern
- Doesn't match any training cluster well
- Negative silhouette score

**Prediction:** Very low confidence (<0.20)

**Teaching Point:** Algorithm correctly refuses to make a confident prediction. This case has an atypical immunoprofile that doesn't fit standard patterns.

**Clinical Action:**
1. Review IHC staining quality (technical artifact?)
2. Repeat questionable stains
3. Mandatory molecular testing
4. Consider diagnoses outside the training differential

---

## Data Generation Details

### Marker Probability Distributions

Based on published literature (Olsen et al. 2006, Modern Pathology):

**Synovial Sarcoma:**
- EMA: 89% positive
- CK7: 52% positive (high specificity when positive)
- bcl-2: 95% positive
- CD99: 75% positive
- S100: 15% positive (usually negative)

**MPNST:**
- S100: 68% positive (characteristic)
- Nestin: 78% positive (high sensitivity)
- NGFR: 82% positive
- EMA: 12% positive (usually negative)

**Ewing Sarcoma:**
- CD99: 93% positive (very high sensitivity)
- Fli-1: 73% positive
- EMA/CK7: <10% positive (usually negative)

### Biological Variability

- Each marker probability includes ±10% random variation
- Ki-67 proliferation index follows normal distributions:
  - Synovial Sarcoma: mean 18% (SD 8%)
  - MPNST: mean 12% (SD 6%)
  - Ewing Sarcoma: mean 45% (SD 15%)

### Outlier Cases

7 atypical cases (~9% of training set) simulate real-world variants:
- Represent known biological variants
- Include diagnostically challenging cases
- Demonstrate algorithm's ability to flag outliers

---

## Troubleshooting

### Problem: "Marker validation shows ERROR"

**Cause:** Likely using query dataset with validation file (has extra column)

**Solution:** Use `sarcoma_ihc_query_realistic.csv` (not the validation version)

---

### Problem: "All predictions have low confidence"

**Cause:** Training file path incorrect or file not found

**Solution:**
1. Verify training CSV file path is correct
2. Use absolute path (full path from root)
3. Put path in quotes if it contains spaces

---

### Problem: "Predicted diagnoses don't match validation file"

**Expected Behavior:** Some mismatches are expected, especially for:
- Atypical cases (Query_009, Query_010, Query_011, Query_012)
- Cases with ambiguous immunoprofiles

**If ALL predictions are wrong:**
- Check that markers are selected in same order
- Verify data types (categorical vs continuous) are correct
- Ensure training file loaded successfully

---

### Problem: "Results differ from expected confidence levels"

**Cause:** Random seed variation in clustering algorithm

**Expected:** Confidence scores may vary by ±0.05 between runs

**Not a problem if:**
- High confidence cases remain high (≥0.70)
- Low confidence cases remain low (<0.30)
- Relative ranking is preserved

---

## Citations

If using these datasets for research or publication:

**Primary Reference:**
Olsen SH, Thomas DG, Lucas DR (2006). Cluster analysis of immunohistochemical profiles in synovial sarcoma, malignant peripheral nerve sheath tumor, and Ewing sarcoma. *Modern Pathology*, 19:659-668.

**Software:**
ClinicoPath R Package for jamovi. Available at: https://github.com/sbalci/ClinicoPathJamoviModule

---

## Additional Resources

- **Step-by-Step Pathologist Guide:** `IHC_PREDICTION_PATHOLOGIST_GUIDE.md`
- **Integration Guide:** `DIAGNOSTIC_PREDICTION_INTEGRATION.md`
- **Diagnostic Clustering Features:** `DIAGNOSTIC_CLUSTERING_GUIDE.md`
- **Spatial Analysis:** `SPATIAL_HETEROGENEITY_GUIDE.md`

---

## Version History

**v1.0 (2025-01-15):**
- Initial release
- 75 training cases, 15 query cases
- Realistic marker distributions
- Includes atypical variants

---

## Contact

For questions or issues with these datasets:
- Open issue on GitHub: https://github.com/sbalci/ClinicoPathJamoviModule/issues
- jamovi community forum: https://forum.jamovi.org

---

**Document Version:** 1.0
**Last Updated:** 2025-01-15
**Dataset Version:** 1.0
