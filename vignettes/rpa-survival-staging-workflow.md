# RPA Survival Staging Workflow: Integration Guide

## Overview

This guide demonstrates how to use three complementary jamovi functions to develop and validate improved cancer staging systems, following the methodology of **Liu et al. (Br J Cancer 2026)**: *"Integrating lymphovascular invasion and ypTNM staging system for esophageal squamous cell carcinoma."*

### The Three-Function Workflow

```
┌─────────────────────────────────────────────────────────────────┐
│                    STAGING DEVELOPMENT WORKFLOW                  │
└─────────────────────────────────────────────────────────────────┘

STEP 1: Develop New Staging System
┌──────────────────────────┐
│   rpasurvival            │  ← Create RPA-based risk groups
│                          │     by integrating predictors
│  Input:                  │     (e.g., LVI + ypTNM stage)
│  - Survival time         │
│  - Event status          │  Output: RPA Stage I, II, III
│  - Predictors (LVI, TNM) │
└──────────────────────────┘
           ↓
STEP 2: Compare Staging Systems (Method A: Groome Criteria)
┌──────────────────────────┐
│   groomecompare          │  ← Compare old vs. new system
│                          │     using 4 Groome metrics
│  Input:                  │
│  - Old system (ypTNM)    │  Output: Radar chart,
│  - New system (RPA)      │          Winner determination
│  - Survival data         │
└──────────────────────────┘
           ↓
STEP 3: Comprehensive Validation (Method B: Advanced Metrics)
┌──────────────────────────┐
│   stagemigration         │  ← Full validation with NRI,
│                          │     IDI, time-ROC, DCA
│  Input:                  │
│  - Old staging           │  Output: Migration matrix,
│  - New staging           │          Bootstrap validation,
│  - Survival outcomes     │          Clinical utility
└──────────────────────────┘
```

---

## STEP 1: Develop RPA Staging System

### Use Case: Esophageal Cancer (Liu et al. 2026)

**Research Question**: Can we improve ypTNM staging by integrating lymphovascular invasion (LVI) status?

**Method**: Use Recursive Partitioning Analysis to create risk groups.

### jamovi Steps:

1. **Open Dataset** containing:
   - `time_months` - Overall survival time
   - `death_event` - Event status (0=censored, 1=death)
   - `yptnm_stage` - Current staging (I, II, III, IVA)
   - `lvi_status` - Lymphovascular invasion (Absent, Present)
   - Other predictors (age, grade, etc.)

2. **Navigate**: Analyses → Survival → RPA for Survival Staging

3. **Configure**:
   ```
   Survival Time:     time_months
   Event Status:      death_event (Event Value = 1)
   Predictor Variab les:
     ✓ yptnm_stage
     ✓ lvi_status
     ✓ (optional: age, grade, etc.)

   Tree Parameters:
     Minimum Node Size:        20
     Complexity Parameter:     0.01
     Maximum Tree Depth:       3
     Cross-Validation Folds:   10
     Prune Tree:              ✓ Yes

   Risk Group Labels:   Automatic (Stage I, II, III)
   Create New Variable: ✓ Yes
   New Variable Name:   rpa_stage
   ```

4. **Interpret Output**:

   **Decision Tree Plot** shows splits:
   ```
   Root (n=565)
   ├─ ypTNM Stage I-II
   │  ├─ LVI Absent → RPA Stage I (n=336, 5-yr OS: 68%)
   │  └─ LVI Present → RPA Stage II (n=27, 5-yr OS: 40%)
   └─ ypTNM Stage III-IVA
      ├─ LVI Absent → RPA Stage II (n=148, 5-yr OS: 49%)
      └─ LVI Present → RPA Stage III (n=54, 5-yr OS: 15%)
   ```

   **Risk Group Summary Table**:
   | Risk Group | N | Events | Median OS | 5-yr OS | 95% CI |
   |---|---|---|---|---|---|
   | RPA Stage I | 336 | 107 | NR | 68% | 63-74% |
   | RPA Stage II | 175 | 93 | 48 | 47% | 39-56% |
   | RPA Stage III | 54 | 35 | 18 | 15% | 7-33% |

   **Kaplan-Meier Plot**: Shows clear separation of 3 risk groups (log-rank p<0.001).

5. **Result**: New variable `rpa_stage` created with 3 risk groups.

---

## STEP 2: Compare Staging Systems (Groome Criteria)

**Research Question**: Is RPA staging superior to ypTNM staging?

### jamovi Steps:

1. **Navigate**: Analyses → Survival → Groome Staging Comparison

2. **Configure**:
   ```
   Survival Time:      time_months
   Event Status:       death_event (Event Value = 1)
   Staging System 1:   yptnm_stage
   Staging System 2:   rpa_stage

   System Names:
     System 1 Name:  ypTNM Stage (8th Edition)
     System 2 Name:  RPA Stage (Proposed)

   Plot Options:
     ✓ Show Radar Chart
     ✓ Show Kaplan-Meier Curves

   Output Options:
     ✓ Detailed Metrics
     ✓ Hazard Ratios
     ✓ Sample Size Distribution
     ✓ C-Index Comparison
   ```

3. **Interpret Results**:

   **Comparison Summary**:
   | Criterion | ypTNM Stage | RPA Stage | Better System |
   |---|---|---|---|
   | Hazard Consistency | 1.549 | 1.451 | RPA Stage |
   | Hazard Discrimination | 1.684 | 1.316 | RPA Stage |
   | Sample Balance | 1.998 | 1.002 | RPA Stage |
   | Outcome Prediction | 1.745 | 1.255 | RPA Stage |
   | **Overall Rank** | **7.976** | **5.024** | **RPA Stage** ✓ |

   **Interpretation**: RPA Stage demonstrates **superior performance** across all 4 Groome criteria. Lower overall rank (5.024 vs. 7.976) indicates better prognostic discrimination.

   **Radar Chart**: Visual comparison showing RPA stage (orange) inside ypTNM stage (blue) on all metrics → RPA wins.

   **C-Index Comparison**:
   | System | C-Index | 95% CI |
   |---|---|---|
   | ypTNM Stage | 0.682 | 0.658-0.706 |
   | RPA Stage | 0.721 | 0.698-0.744 |

   **Difference**: ΔC = 0.039 (clinically meaningful if >0.05, borderline here).

---

## STEP 3: Comprehensive Validation (Stage Migration Analysis)

**Research Question**: Quantify improvement using advanced metrics (NRI, IDI, clinical utility).

### jamovi Steps:

1. **Navigate**: Analyses → OncoPathT → Stage Migration Analysis

2. **Configure**:
   ```
   Old Staging System:   yptnm_stage
   New Staging System:   rpa_stage
   Survival Time:        time_months
   Event Status:         death_event (Event Level = 1)

   Analysis Type:  Comprehensive

   Advanced Metrics:
     ✓ Calculate NRI (Net Reclassification Improvement)
     ✓ Calculate IDI (Integrated Discrimination Improvement)
     ✓ Time-Dependent ROC Analysis
     ✓ Decision Curve Analysis
     ✓ C-Index with Bootstrap CI

   Validation:
     ✓ Perform Bootstrap Validation
     Bootstrap Replicates: 1000
     Random Seed: 12345

   Time Points for ROC:
     1, 3, 5 years
   ```

3. **Interpret Advanced Metrics**:

   **A. Migration Matrix**:
   ```
   Old (ypTNM) → New (RPA)
   ──────────────────────────
   Stage I   → 90% RPA I,  10% RPA II
   Stage II  → 40% RPA II, 60% RPA III
   Stage III → 80% RPA II, 20% RPA III
   Stage IVA → 100% RPA III
   ```

   **B. Net Reclassification Improvement (NRI)**:
   | Category | NRI | 95% CI | p-value |
   |---|---|---|---|
   | Events | 0.18 | 0.11-0.25 | <0.001 |
   | Non-events | 0.12 | 0.06-0.18 | <0.001 |
   | **Total NRI** | **0.30** | **0.22-0.38** | **<0.001** |

   **Interpretation**: 30% net improvement in risk classification. Events correctly reclassified upward (higher risk) and non-events downward (lower risk).

   **C. Integrated Discrimination Improvement (IDI)**:
   | Metric | Value | 95% CI | p-value |
   |---|---|---|---|
   | IDI | 0.085 | 0.062-0.108 | <0.001 |

   **Interpretation**: 8.5% improvement in integrated discrimination. Clinically meaningful (>5%).

   **D. Time-Dependent ROC**:
   | Time Point | ypTNM AUC | RPA AUC | Δ AUC | DeLong p |
   |---|---|---|---|---|
   | 1 year | 0.68 | 0.73 | +0.05 | 0.021 |
   | 3 years | 0.71 | 0.76 | +0.05 | 0.015 |
   | 5 years | 0.69 | 0.74 | +0.05 | 0.018 |

   **Interpretation**: Consistent +5% improvement in AUC across all time points.

   **E. Decision Curve Analysis**:
   - Net benefit curve shows RPA staging superior to ypTNM across threshold probabilities 10-80%.
   - Maximum net benefit at 30% threshold: RPA = 0.42, ypTNM = 0.35.

   **F. Bootstrap Validation** (1000 replicates):
   | Metric | Apparent | Optimism | Corrected |
   |---|---|---|---|
   | C-index (ypTNM) | 0.682 | 0.012 | 0.670 |
   | C-index (RPA) | 0.721 | 0.015 | 0.706 |

   **Interpretation**: Minimal optimism (<0.02) indicates stable performance. RPA maintains superiority after bias correction.

---

## Clinical Decision Framework

### Evidence Summary

**Question**: Should we adopt RPA staging over ypTNM staging?

**Evidence Strength**: ⭐⭐⭐⭐⭐ (Strong)

| Evidence Type | Result | Threshold | Meets? |
|---|---|---|---|
| **Groome Overall Rank** | RPA: 5.024 vs. ypTNM: 7.976 | Lower is better | ✓ Yes |
| **C-Index Improvement** | ΔC = +0.039 | >0.05 ideal | ~ Borderline |
| **NRI** | +0.30 (30%) | >0.20 meaningful | ✓ Yes |
| **IDI** | +0.085 (8.5%) | >0.05 meaningful | ✓ Yes |
| **Time-ROC (5-yr)** | ΔAUC = +0.05 | >0.03 meaningful | ✓ Yes |
| **Clinical Utility (DCA)** | Net benefit higher | Visual superiority | ✓ Yes |
| **Bootstrap Validation** | Optimism <0.02 | <0.03 acceptable | ✓ Yes |

### Recommendation

**✓ ADOPT RPA STAGING**

**Rationale**:
1. **Statistical Superiority**: All metrics favor RPA staging (Groome, NRI, IDI, ROC).
2. **Clinical Utility**: Decision curve analysis confirms practical benefit.
3. **Validation**: Bootstrap validation shows stable, reproducible performance.
4. **Interpretability**: 3 risk groups (vs. 4 in ypTNM) simplify clinical decisions.
5. **Integration**: Incorporates biologically relevant factor (LVI) that was independent predictor in multivariate analysis (HR=1.70, p<0.001).

**Implementation Notes**:
- RPA staging requires pathology report of LVI status (H&E ± IHC with D2-40, CD31, CD34).
- Best suited for post-neoadjuvant CRT setting (not treatment-naïve).
- External validation recommended before widespread adoption.

---

## Statistical Notes

### When to Use Each Function

| Function | Purpose | Use When | Output |
|---|---|---|---|
| **rpasurvival** | Develop new staging | You want to CREATE risk groups from predictors | Decision tree, KM curves, new variable |
| **groomecompare** | Quick comparison | You want FAST assessment of 2 staging systems | 4 Groome metrics, radar chart |
| **stagemigration** | Full validation | You need COMPREHENSIVE evidence for publication/clinical adoption | NRI, IDI, time-ROC, DCA, bootstrap |

### Typical Workflow

**Exploratory Phase** (Discovery):
1. Use `rpasurvival` to develop RPA staging
2. Use `groomecompare` for quick assessment

**Validation Phase** (Confirmation):
3. Use `stagemigration` for comprehensive validation
4. External cohort validation (repeat steps 2-3)

**Implementation Phase** (Dissemination):
5. Document methodology (save jamovi .omv file)
6. Create clinical implementation guide
7. Submit for publication with full metrics

---

## Example Results Narrative (for Publications)

### Methods Section

> **Recursive Partitioning Analysis**: We developed a modified post-neoadjuvant pathologic staging (RPA staging) system using recursive partitioning analysis (CART for survival data) in the training cohort (n=565). Predictors included ypTNM stage and lymphovascular invasion (LVI) status. The tree was grown using log-rank split criterion with minimum node size of 20 and complexity parameter of 0.01, then pruned using 10-fold cross-validation. Terminal nodes were assigned to risk groups (RPA Stage I, II, III).

> **Staging System Comparison**: Performance of RPA staging vs. ypTNM staging was compared using Groome criteria (hazard consistency, discrimination, balance, outcome prediction) and additional metrics including Harrell's C-index, Net Reclassification Improvement (NRI), Integrated Discrimination Improvement (IDI), time-dependent ROC analysis, and decision curve analysis. Internal validation was performed using 1000 bootstrap replicates. External validation was conducted in an independent cohort (n=366).

### Results Section

> **RPA Staging Development**: Recursive partitioning analysis classified patients into three prognostic groups: RPA Stage I (ypTNM I-II without LVI, n=336, 5-year OS 68%), RPA Stage II (ypTNM I-II with LVI or III without LVI, n=175, 5-year OS 47%), and RPA Stage III (ypTNM III with LVI or IVA, n=54, 5-year OS 15%) (log-rank p<0.001).

> **Staging Performance Comparison**: RPA staging demonstrated superior performance over ypTNM staging across all Groome criteria (overall rank: 5.024 vs. 7.976). C-index was higher for RPA staging (0.721 vs. 0.682, difference 0.039, 95% CI 0.015-0.063). Net Reclassification Improvement was 0.30 (95% CI 0.22-0.38, p<0.001), and Integrated Discrimination Improvement was 0.085 (95% CI 0.062-0.108, p<0.001). Decision curve analysis confirmed superior clinical utility of RPA staging across threshold probabilities 10-80%.

> **External Validation**: In the validation cohort (n=366), RPA staging maintained superior performance (overall Groome rank: 5.1 vs. 8.2; C-index: 0.709 vs. 0.674). Bootstrap validation (1000 replicates) showed minimal optimism (0.015), indicating stable performance.

---

## Troubleshooting

### Common Issues

**1. RPA creates too many/few groups**:
- **Too many**: Increase `minbucket` (e.g., 30, 50) or `cp` (e.g., 0.05)
- **Too few**: Decrease `minbucket` (e.g., 10, 15) or `cp` (e.g., 0.001); increase `maxdepth`

**2. Tree has no splits**:
- Predictors may not have sufficient discrimination
- Try adding more predictors or reducing `minbucket`
- Check if sample size is adequate (need 50+ events)

**3. Groome metrics look unusual**:
- Check staging variables are ordered correctly (Stage I < II < III)
- Ensure adequate events per stage (need 10+ per group)

**4. C-index difference is small (<0.03)**:
- May still be statistically significant if sample is large
- Check NRI/IDI for clinical meaningfulness
- Consider if improvement justifies implementation effort

**5. Bootstrap takes too long**:
- Reduce `nboot` to 500 for initial exploration
- Use 1000+ for final validation
- Consider parallel processing if available

---

## References

### Primary Citation
Liu S, et al. Integrating lymphovascular invasion and ypTNM staging system for esophageal squamous cell carcinoma undergoing neoadjuvant chemoradiotherapy and surgery: a multi-institutional analysis. *Br J Cancer.* 2026;134:608-617.

### Methodological References
1. **Groome Criteria**: Groome PA, et al. A comparison of published head and neck stage groupings. *Head Neck.* 2001;23:613-24.
2. **RPA**: Xie Y, et al. autoRPA: a web server for constructing cancer staging models by recursive partitioning analysis. *Comput Struct Biotechnol J.* 2020;18:3361-7.
3. **NRI/IDI**: Pencina MJ, et al. Evaluating the added predictive ability of a new marker. *Stat Med.* 2008;27:157-72.
4. **DCA**: Vickers AJ, et al. Decision curve analysis: a novel method for evaluating prediction models. *Med Decis Making.* 2006;26:565-74.
5. **Bootstrap Validation**: Steyerberg EW. *Clinical Prediction Models.* 2nd ed. Springer; 2019.

---

## Software Citation

When publishing results using these functions, please cite:

> Statistical analyses were performed using jamovi (The jamovi project, 2024) with the ClinicoPath module (version 0.0.33). Recursive partitioning analysis was performed using the rpart package (Therneau & Atkinson, 2019), Groome staging comparison used survival package (Therneau, 2024), and comprehensive staging validation employed survIDINRI (Uno et al., 2013) and DecisionCurve (Brown, 2020) packages.

---

**Document Version**: 1.0
**Last Updated**: 2026-01-31
**Contact**: ClinicoPath Development Team
