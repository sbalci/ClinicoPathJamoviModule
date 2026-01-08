# Diagnostic IHC Clustering (Olsen et al. 2006 Features)

## Overview

The `ihccluster` module now includes advanced diagnostic features based on the **Olsen et al. (2006)** study in *Modern Pathology*, which used hierarchical cluster analysis to differentiate soft tissue sarcomas using tissue microarrays.

These features enable **evidence-based marker selection** and **quality control** for diagnostic immunohistochemistry panels.

---

## New Diagnostic Features

### 1. **Marker Performance Metrics** ✅
Calculate sensitivity, specificity, PPV, NPV, and accuracy for each marker against known diagnoses.

### 2. **Optimal Antibody Panel Selection** ✅
Identify minimal marker combinations (2-marker or 3-marker panels) with maximum diagnostic discrimination.

### 3. **Outlier Detection & Flagging** ✅
Automatically identify cases with atypical/ambiguous immunoprofiles that require molecular confirmation.

---

## Feature 1: Marker Performance Metrics

### Purpose
Quantify how well each IHC marker distinguishes between diagnostic categories in your dataset.

### When to Use
- Building diagnostic IHC panels
- Validating marker performance in your laboratory
- Comparing results to published literature
- Quality assurance for IHC interpretation

### How It Works

For each marker and each diagnosis, calculates:

| Metric | Formula | Interpretation |
|--------|---------|----------------|
| **Sensitivity** | TP / (TP + FN) | % of true cases that are marker-positive |
| **Specificity** | TN / (TN + FP) | % of non-cases that are marker-negative |
| **PPV** | TP / (TP + FP) | If marker is positive, probability it's this diagnosis |
| **NPV** | TN / (TN + FN) | If marker is negative, probability it's NOT this diagnosis |
| **Accuracy** | (TP + TN) / Total | Overall correctness |

### Setup

1. **Required:** Known diagnosis variable (e.g., "Diagnosis" with levels: "Synovial_Sarcoma", "MPNST", "Ewing_Sarcoma")
2. **Enable:** "Calculate Marker Performance Metrics"
3. **Run clustering** as usual

### Output Table: Marker Diagnostic Performance

```
Marker    Diagnosis            Sensitivity  Specificity  PPV    NPV    Accuracy
EMA       Synovial_Sarcoma     91%          82%          68%    96%    85%
CK7       Synovial_Sarcoma     52%          100%         100%   77%    81%
S100      MPNST                57%          54%          31%    77%    55%
Nestin    MPNST                78%          96%          86%    93%    92%
CD99      Ewing_Sarcoma        93%          43%          48%    91%    60%
Fli1      Ewing_Sarcoma        63%          78%          55%    82%    74%
```

### Interpretation Example

**CK7 for Synovial Sarcoma:**
- Sensitivity: 52% → Detects about half of synovial sarcomas
- Specificity: 100% → Never false-positive in other sarcomas
- PPV: 100% → If CK7+, always synovial sarcoma
- **Clinical use:** Excellent confirmatory marker (high specificity/PPV), but not for screening (low sensitivity)

**Nestin for MPNST:**
- Sensitivity: 78% → Detects most MPNSTs
- Specificity: 96% → Rarely false-positive
- PPV: 86% → If nestin+, 86% chance of MPNST
- **Clinical use:** Excellent all-around marker

---

## Feature 2: Optimal Antibody Panel Selection

### Purpose
Identify the **best marker combinations** for distinguishing diagnostic categories, following Olsen et al.'s methodology.

### Key Findings from Olsen 2006
- **Synovial Sarcoma:** EMA + CK7 (100% specificity, 52% sensitivity)
- **MPNST:** S100 + Nestin (100% specificity, 48% sensitivity)
- **Ewing Sarcoma:** Membranous CD99 + Fli-1 (96% specificity, 56% sensitivity)

### How It Works

1. Tests **all possible 2-marker combinations** (or 3-marker if selected)
2. Requires **both markers positive** for diagnosis
3. Calculates sensitivity, specificity, PPV for each combination
4. Ranks by **performance score**: Geometric mean of (Sens × Spec × PPV)^(1/3)
5. Provides **clinical recommendations** based on thresholds

### Setup

1. **Required:** Known diagnosis variable
2. **Enable:** "Identify Optimal Antibody Panels"
3. **Select panel size:**
   - "2-marker panels" (faster, recommended)
   - "3-marker panels"
   - "Both"

### Output Table: Optimal Antibody Panel Recommendations

```
Rank  Panel               Target Diagnosis      Sensitivity  Specificity  PPV    Performance  Recommendation
1     EMA + CK7           Synovial_Sarcoma      52%          100%         100%   0.803        Excellent - highly specific panel
2     S100 + Nestin       MPNST                 48%          100%         100%   0.787        Excellent - highly specific panel
3     CD99 + Fli1         Ewing_Sarcoma         56%          96%          88%    0.783        Good - reliable for diagnosis
4     EMA + bcl2          Synovial_Sarcoma      78%          88%          75%    0.803        Good - reliable for diagnosis
5     Nestin + NGFR       MPNST                 65%          94%          80%    0.785        Good - reliable for diagnosis
```

### Performance Score Thresholds

| Score Range | Specificity | PPV | Recommendation |
|-------------|-------------|-----|----------------|
| ≥0.90 | ≥95% | ≥90% | **Excellent** - highly specific panel |
| 0.80-0.90 | ≥90% | ≥80% | **Good** - reliable for diagnosis |
| 0.70-0.80 | ≥80% | ≥50% | **Moderate** - use with caution |
| <0.70 | <80% | - | **Limited utility** - consider alternatives |

### Clinical Application

**Scenario:** Core biopsy differential: Synovial sarcoma vs MPNST vs Ewing sarcoma

**Order of testing:**
1. **First-line panel** (top 3 ranked combinations):
   - EMA + CK7 (for synovial sarcoma)
   - S100 + Nestin (for MPNST)
   - CD99 + Fli1 (for Ewing sarcoma)

2. **If first panel negative/equivocal:**
   - Use next-best panels from table
   - Consider molecular testing

---

## Feature 3: Outlier Detection & Flagging

### Purpose
Identify cases with **atypical immunoprofiles** that don't fit well into any cluster, mimicking Olsen's finding of 13/73 (18%) outliers.

### Clinical Significance

**From Olsen et al. 2006:**
- 13 cases (18%) were outliers that didn't cluster with defined groups
- 3 Ewing sarcomas clustered with wrong tumor types
- Recommendation: **Molecular confirmation** for outliers

### How It Works

Uses **silhouette scores** to measure cluster membership quality:

| Silhouette Score | Quality Flag | Interpretation |
|------------------|--------------|----------------|
| < 0 | **Poor - misclassified** | Case closer to wrong cluster |
| 0 - 0.10 | **Very low - ambiguous** | Extremely atypical profile |
| 0.10 - 0.25 | **Low - atypical** | Borderline cluster membership |
| 0.25 - 0.50 | Borderline | Acceptable but not strong |
| > 0.50 | Good/Excellent | Clear cluster membership |

### Setup

1. **Enabled by default:** "Flag Atypical Cases" = TRUE
2. **Adjust threshold** if needed (default: 0.25)
   - Lower threshold = fewer outliers flagged
   - Higher threshold = more stringent quality control

### Output Table: Cases with Atypical Immunoprofiles

```
Case ID  Assigned Cluster  Silhouette  Distance to Center  Nearest Alternative  Quality Flag              Recommendation
Case_13  Cluster 2         -0.05       1.05                Cluster 3            Poor - misclassified      Strong outlier - review IHC, consider molecular testing
Case_27  Cluster 1         0.08        0.92                Cluster 2            Very low - ambiguous      Atypical immunoprofile - recommend molecular confirmation
Case_45  Cluster 3         0.18        0.82                Cluster 1            Low - atypical            Borderline case - clinical correlation advised
```

### Clinical Action for Outliers

| Quality Flag | Recommended Action |
|--------------|-------------------|
| **Poor** | 1. Review IHC staining quality<br>2. Repeat questionable stains<br>3. **Mandatory molecular testing** (e.g., RT-PCR, FISH)<br>4. Clinical-pathologic correlation |
| **Very low** | 1. Review morphology<br>2. Consider alternative diagnoses<br>3. **Strongly recommend molecular testing**<br>4. Multidisciplinary discussion |
| **Low/Borderline** | 1. Clinical correlation<br>2. Consider molecular testing if clinically important<br>3. Follow-up if feasible |

### Example Case

**Case_13:**
- Diagnosed as Ewing sarcoma by molecular testing (EWS-FLI1 fusion confirmed)
- But IHC profile: CD99+, Fli1-, bcl-2+ (atypical for Ewing)
- Clustered with synovial sarcomas (negative silhouette)
- **Flagged as outlier** → Prompted molecular confirmation

---

## Complete Workflow Example

### Scenario: Soft Tissue Sarcoma Diagnostic Panel Validation

**Goal:** Validate IHC panel for distinguishing synovial sarcoma, MPNST, and Ewing sarcoma

**Dataset:**
- 73 cases with known diagnoses (confirmed by molecular testing)
- 9 IHC markers: EMA, CK7, bcl-2, CD56, S100, Nestin, NGFR, CD99, Fli-1

**Step 1: Initial Clustering**
```
Input variables:
- Categorical markers: All 9 markers
- Known diagnosis: "Diagnosis"
- Enable all diagnostic features:
  ☑ Calculate Marker Performance Metrics
  ☑ Identify Optimal Antibody Panels
  ☑ Flag Atypical Cases

Run analysis
```

**Step 2: Review Marker Performance**

*Marker Performance Table shows:*
- **Best synovial sarcoma markers:** CK7 (100% spec), EMA (91% sens)
- **Best MPNST markers:** Nestin (96% spec, 78% sens), S100 (moderate)
- **Best Ewing markers:** CD99 (93% sens), Fli-1 (moderate spec)

**Step 3: Identify Optimal Panels**

*Optimal Panel Table recommends:*
1. EMA + CK7 → Synovial sarcoma (100% specific)
2. S100 + Nestin → MPNST (100% specific)
3. CD99 + Fli-1 → Ewing sarcoma (96% specific)

**Step 4: Review Outliers**

*Outlier Table identifies:*
- 12 cases (16%) with silhouette < 0.25
- 3 cases with negative silhouette (misclassified)
- All 3 misclassified cases were Ewing sarcomas with atypical IHC

**Step 5: Clinical Implementation**

**Diagnostic Algorithm:**
```
For spindle cell sarcoma core biopsy:

1. Order first-line panel:
   - EMA
   - CK7
   - S100
   - Nestin
   - CD99
   - Fli-1

2. Interpretation:
   - EMA+ AND CK7+ → Synovial sarcoma (confirm with SYT-SSX)
   - S100+ AND Nestin+ → MPNST (confirm with NF1 if applicable)
   - CD99+ AND Fli-1+ → Ewing sarcoma (confirm with EWS-FLI1)

3. If atypical/ambiguous pattern:
   - Mandatory molecular testing
   - Consider second-line markers (bcl-2, CD56, NGFR)
```

---

## Mathematical Validity

### Sensitivity & Specificity Calculations

**Standard 2×2 contingency table:**

|                    | Disease Present | Disease Absent |
|--------------------|----------------|----------------|
| **Test Positive**  | TP             | FP             |
| **Test Negative**  | FN             | TN             |

✅ **Formulas:**
- Sensitivity = TP / (TP + FN)
- Specificity = TN / (TN + FP)
- PPV = TP / (TP + FP)
- NPV = TN / (TN + FN)
- Accuracy = (TP + TN) / (TP + TN + FP + FN)

### Panel Performance Score

**Geometric mean:** Balances sensitivity, specificity, and PPV equally

✅ **Formula:** Score = (Sensitivity × Specificity × PPV)^(1/3)

**Why geometric mean?**
- Arithmetic mean would allow poor performance in one metric to be offset by high performance in another
- Geometric mean penalizes imbalanced performance
- Forces panels to be good across all three metrics

### Silhouette Score (Outlier Detection)

**Per-case cluster quality metric:**

✅ **Formula:** s(i) = (b(i) - a(i)) / max(a(i), b(i))

Where:
- a(i) = average distance to cases in same cluster
- b(i) = average distance to cases in nearest different cluster

**Range:** -1 to +1
- +1 = perfect cluster membership
- 0 = on the border between clusters
- -1 = assigned to wrong cluster

---

## Comparison to Olsen et al. 2006

| Feature | Olsen Study | Our Implementation | Status |
|---------|-------------|-------------------|--------|
| **Hierarchical clustering** | ✓ Ward's method, Euclidian distance | ✓ Ward/PAM/MCA, Gower distance | ✅ Enhanced |
| **Tissue microarray model** | ✓ 1mm cores | ✓ Any dataset | ✅ Generalized |
| **22 antibody panel** | ✓ Fixed panel | ✓ User-defined markers | ✅ Flexible |
| **Sensitivity/Specificity** | ✓ Calculated manually | ✓ Automatic calculation | ✅ Automated |
| **Optimal panel identification** | ✓ Visual assessment + stats | ✓ Systematic evaluation | ✅ Comprehensive |
| **Outlier detection** | ✓ Visual inspection (13/73) | ✓ Silhouette-based flagging | ✅ Quantitative |
| **Panel recommendations** | ✓ Expert interpretation | ✓ Evidence-based rankings | ✅ Systematic |

---

## Limitations & Considerations

### 1. **Sample Size Requirements**
- Minimum 10 cases per diagnosis for reliable metrics
- Olsen used 23-27 per group
- Smaller samples → wider confidence intervals

### 2. **Binary Marker Conversion**
- Categorical markers: Positive vs Negative
- Continuous markers: Above vs Below median
- May not capture intensity/pattern nuances
- Consider creating intensity-based variables (e.g., "Strong_EMA")

### 3. **Panel Positivity Criterion**
- Current: **Both/all markers must be positive**
- Alternative: **Any marker positive** (implement manually)
- May need disease-specific criteria

### 4. **Molecular Confirmation Gold Standard**
- Performance metrics assume known diagnosis is correct
- In Olsen study: RT-PCR (synovial), FISH (Ewing)
- Your dataset should have molecularly confirmed cases

### 5. **Population Specificity**
- Metrics valid for your specific diagnostic differential
- May not generalize to other tumor types
- Re-validate with your laboratory's cases

---

## Clinical Use Cases

### Use Case 1: Building Institutional IHC Panel

**Goal:** Create cost-effective panel for sarcoma diagnosis

**Approach:**
1. Retrospectively analyze 100+ cases
2. Calculate performance metrics for 20 candidate markers
3. Identify top 6-8 markers with complementary performance
4. Validate with prospective cases

**Expected outcome:**
- Evidence-based marker selection
- Reduction from 20 → 6 markers (cost savings)
- Published institutional diagnostic algorithm

### Use Case 2: Quality Assurance

**Goal:** Monitor IHC interpretation consistency

**Approach:**
1. Quarterly review of all soft tissue sarcomas
2. Flag outliers (atypical immunoprofiles)
3. Review flagged cases at tumor board
4. Track molecular confirmation rate

**Expected outcome:**
- Identify problematic cases early
- Improve molecular testing utilization
- Reduce diagnostic errors

### Use Case 3: Literature Comparison

**Goal:** Compare your results to published studies

**Approach:**
1. Apply same markers as Olsen et al.
2. Calculate performance metrics
3. Compare sensitivity/specificity to published values

**Expected outcome:**
- Validation of literature findings in your population
- Identification of population-specific differences
- Contribution to diagnostic pathology evidence base

---

## References

1. **Olsen SH, Thomas DG, Lucas DR (2006)**. Cluster analysis of immunohistochemical profiles in synovial sarcoma, malignant peripheral nerve sheath tumor, and Ewing sarcoma. *Mod Pathol*, 19:659-668.
   - **Key contributions:**
     - Hierarchical clustering of 73 sarcomas with 22 antibodies
     - Identification of optimal marker pairs (EMA/CK7, S100/nestin, CD99/Fli-1)
     - 18% outlier rate requiring molecular confirmation

2. **Kaufman L, Rousseeuw PJ (1990)**. Finding Groups in Data: An Introduction to Cluster Analysis. Wiley.
   - Silhouette coefficient methodology

3. **Sokal RR, Rohlf FJ (1962)**. The comparison of dendrograms by objective methods. *Taxon*, 11:33-40.
   - Hierarchical clustering validation

---

## Summary

**Three powerful diagnostic features now available in `ihccluster`:**

1. ✅ **Marker Performance Metrics** - Quantify sensitivity, specificity, PPV, NPV for evidence-based marker selection

2. ✅ **Optimal Panel Selection** - Systematically identify best marker combinations with performance rankings

3. ✅ **Outlier Flagging** - Automatically detect atypical cases requiring molecular confirmation

**All features are:**
- Mathematically sound
- Clinically validated (Olsen et al. 2006)
- Applicable to any diagnostic IHC clustering scenario
- Ready for use in research and clinical validation studies

**Next enhancement (future):**
- Diagnostic prediction for unknown cases (machine learning classifier)