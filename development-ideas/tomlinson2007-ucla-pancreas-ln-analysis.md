# Tomlinson et al. 2007 - UCLA Pancreatic Cancer LN Adequacy Analysis

**Citation**: Tomlinson JS, Jain S, Bentrem DJ, et al. Accuracy of Staging Node-Negative Pancreas Cancer: A Potential Quality Measure. *Arch Surg.* 2007;142(8):767-774.

**Institution**: University of California Los Angeles (UCLA), Veterans Affairs Greater Los Angeles Healthcare System

**Study Period**: 1988-2002 (SEER database)

---

## Study Overview

### Research Question
**What is the optimal number of lymph nodes to examine for accurate staging of node-negative pancreatic adenocarcinoma after pancreaticoduodenectomy?**

### Study Design
- **Database**: SEER (Surveillance, Epidemiology, and End Results) national cancer registry
- **Coverage**: 26% of US population, 13 regional registries
- **Total cohort**: 3,505 patients who underwent pancreaticoduodenectomy for pancreatic adenocarcinoma
- **Analysis cohorts**:
  - **pN0 cohort**: 1,150 patients (33% of total) - pathologically node-negative
  - **pN1a cohort**: 584 patients (17% of total) - single positive node
  - Perioperative deaths excluded
  - Patients with >1 positive node excluded

### Key Demographics
- Median age: 64.4 years (pN0: 64.6y, pN1a: 63.4y)
- Sex: 49.3% female
- Race: 80.6% white, 11.0% black, 7.9% Asian
- T stage: ~70% were T3 (tumor extends beyond pancreas)
- AJCC/UICC 6th edition staging

---

## Methodology

### Statistical Approach - Four Complementary Analyses

#### 1. Univariate Survival Analysis (Lymph Node Cut Points, LNCP 2-25)
- **LNCP definition**: Threshold number of nodes examined (dichotomous variable)
- Example: LNCP=15 divides pN0 into <15 vs ≥15 examined nodes
- Generated Kaplan-Meier curves for each LNCP
- Log-rank comparison to maximize χ² statistic

#### 2. Multivariate Cox Regression (Validation)
- **Dependent variable**: Overall survival (surgery → death/last follow-up)
- **Independent variables**:
  - Age
  - Sex
  - T stage
  - Number of LN examined (dichotomous, based on optimal LNCP)

#### 3. Continuous Variable Cox Regression (Marginal Benefit)
- Number of LN examined as **continuous variable** (log-transformed due to skewed distribution)
- Predicted survival benefit per additional node
- Calculated individual predicted hazard ratios

#### 4. pN1a Cohort Analysis (Detection Sensitivity)
- Plotted # nodes examined vs cumulative frequency
- Determined nodes needed to identify 90% of single-positive-node cases
- **Rationale**: Smallest nodal burden = most sensitive test of adequacy

---

## Key Findings

### Primary Results

| Metric | Finding |
|--------|---------|
| **Optimal LNCP** | **15 lymph nodes** |
| Median # examined | 7 nodes (range: 1-54) |
| % achieving ≥15 LN | **Only 19%** (995/1150 had <15 examined) |
| Median survival (≥15 LN) | 27 months |
| Median survival (<15 LN) | 19 months |
| **Survival difference** | **8 months** (P<0.001) |
| Log-rank χ² statistic | 14.49 (maximum across all LNCPs) |

### Univariate Analysis Results (Representative LNCPs)

| LNCP | <LNCP | ≥LNCP | Median Survival <LNCP | Median Survival ≥LNCP | Difference | χ² | P-value |
|------|-------|-------|----------------------|----------------------|------------|-----|---------|
| 5 | 403 (35%) | 747 (65%) | 19 mo | 20 mo | 1 mo | 3.17 | 0.08 |
| 10 | 771 (67%) | 379 (33%) | 19 mo | 22 mo | 3 mo | 7.64 | 0.006 |
| **15** | **995 (87%)** | **155 (13%)** | **19 mo** | **27 mo** | **8 mo** | **14.49** | **0.0001** |
| 20 | 1083 (94%) | 67 (6%) | 20 mo | 30 mo | 10 mo | 6.46 | 0.01 |
| 25 | 1123 (98%) | 27 (2%) | 20 mo | 22 mo | 2 mo | 3.56 | 0.06 |

**Key Observation**: LNCP=15 showed **most statistically significant survival difference** (maximum χ²=14.49, P=0.0001)

### Multivariate Cox Regression (Validation of LNCP=15)

| Variable | Hazard Ratio (95% CI) | P-value | Interpretation |
|----------|----------------------|---------|----------------|
| **LNCP ≥15 vs <15** | **0.63 (0.49-0.80)** | **<0.0001** | **37% mortality reduction** |
| Age (per year) | 1.014 (1.007-1.021) | <0.0001 | 1.4% increase per year |
| Female vs Male | 1.217 (1.047-1.415) | 0.01 | 22% higher risk |
| T2 vs T1 | 1.525 (1.144-2.031) | 0.004 | 53% higher risk |
| T3 vs T1 | 1.859 (1.426-2.423) | <0.0001 | 86% higher risk |
| T4 vs T1 | 2.081 (1.162-3.728) | 0.01 | 108% higher risk |

**Interpretation**: Having ≥15 nodes examined is a **strong independent predictor** of survival (HR=0.63 = 30% increased likelihood of survival).

### Continuous Variable Model (Marginal Benefit Curve)

- **HR per additional node**: 0.88 (95% CI: 0.81-0.95, P=0.002)
- **Incremental benefit**: Each additional examined node = 12% mortality reduction (1 - 0.88 = 0.12)

**Predicted Survival Benefit** (relative to 1 node examined):
- 1 node → 5 nodes: HR 0.87 → 0.78 (9% improvement)
- 5 nodes → 10 nodes: HR 0.78 → 0.73 (5% improvement)
- 10 nodes → 15 nodes: HR 0.73 → 0.71 (2% improvement)
- 15 nodes → 20 nodes: HR 0.71 → 0.69 (**only 3% marginal benefit**)

**Clinical Significance**: **Diminishing returns after 15 nodes** - minimal survival benefit for examining >15 nodes.

### pN1a Cohort Analysis (Detection Sensitivity)

- **90% of single-positive-node disease detected with examination of ≤15 nodes**
- Cumulative detection curve shows steep rise from 1-15 nodes, then plateau
- This validates that 15 nodes provide sufficient sampling to detect minimal nodal burden

---

## Statistical Rigor & Validation

### Convergence of Four Analyses on N=15

| Analysis Method | Conclusion Supporting N=15 |
|----------------|---------------------------|
| 1. Univariate LNCP | Maximum χ²=14.49 at LNCP=15, P<0.0001 |
| 2. Multivariate Cox (dichotomous) | HR=0.63 (P<0.0001), independent predictor |
| 3. Continuous variable model | Marginal benefit plateaus after 15 nodes |
| 4. pN1a detection | 90% of single-node metastases found with ≤15 |

### Comparison to Other Series

| Institution | Median/Mean # LN Examined | Study Type |
|-------------|-------------------------|------------|
| **SEER (Tomlinson 2007)** | **Median 7** (only 19% ≥15) | Population-based (n=1,150) |
| Memorial Sloan-Kettering (Brennan 2004) | Median 15 | Single institution (n=616) |
| Johns Hopkins (Yeo 2002) | Mean 16 | Randomized trial (86% pylorus-preserving) |
| Fox Chase (Berger 2004) | Median 17 | Single institution |
| Royal College Pathologists (UK) | 10-20 nodes | Guideline |

**Interpretation**: Major academic centers routinely achieve ≥15 nodes, suggesting **15 is realistic and achievable**.

---

## Mechanistic Interpretation

### Stage Migration vs Therapeutic Effect

**Authors' Argument (Stage Migration)**:
- Removing more **negative nodes** from truly node-negative patients = no therapeutic benefit
- Superior survival with more examined nodes = **understaging** of inadequately sampled cases
- Inadequate sampling misses low-burden nodal disease → false pN0 classification → worse survival

**Alternative Hypothesis (Micrometastasis Resection)**:
- Discussed but considered less likely
- Would require undetected micrometastases removed during extended dissection
- Authors note: requires further study but "stage migration definitely playing a role"

### Factors Affecting Node Yield

1. **Surgical factors**:
   - Type of operation (standard vs pylorus-preserving Whipple)
   - Extent of lymphadenectomy
   - Pylorus-preserving removes lesser/greater curve gastric LN

2. **Specimen factors**:
   - Regional node variability between patients
   - Small bowel mesentery (minimal in standard resection)

3. **Pathology factors**:
   - Technique of pathologist examining specimen
   - Time spent on dissection
   - Use of fat-clearing solutions

**Clinical Reality**: Both surgery AND pathology contribute to node yield (per Dr. Ko in discussion).

---

## Clinical Implications

### Quality Measure Proposal

**Tomlinson 2007 proposes examination of ≥15 lymph nodes as quality indicator for pancreatic cancer surgery**

**Rationale**:
1. **Prognostic accuracy**: 8-month median survival difference
2. **Staging accuracy**: Prevents understaging that confounds trials
3. **Achievability**: Major centers routinely achieve this
4. **Room for improvement**: Only 19% of SEER cases met threshold (median=7)

### Impact on Clinical Trials

**Historical Context** - Gastrointestinal Tumor Study Group (GITSG) Trial:
- 49 patients randomized (adjuvant chemoRT vs observation)
- 72% were pathologically node-negative
- Reported ~9-month median survival improvement with adjuvant therapy
- **Concern**: Inadequate nodal sampling → maldistribution of true pN0 (27-month survival) vs understaged pN1 (19-month survival) → confounded results

**Meta-analysis of Adjuvant Trials** (Stocken 2005):
- ~50% of patients enrolled were node-negative
- Without standardization, cannot rule out staging inaccuracy confounding results

---

## Comparison to Colon Cancer Literature

### Established Colon Cancer Guidelines
- **AJCC/UICC**: Minimum 12 lymph nodes for adequate staging of node-negative colon cancer
- **National Quality Forum**: Endorsed as accountability measure (pay-for-performance)
- Multiple studies validated stage migration effect (Smith 2005, Cserni 2002, Swanson 2003)

### Pancreatic Cancer Parallel
- Tomlinson 2007 applies similar methodology to pancreas cancer
- **Proposed threshold**: 15 lymph nodes (vs 12 for colon)
- Rationale for higher threshold:
  - More aggressive biology
  - Lower overall LN yield in pancreatic specimens
  - Need for 90% detection of pN1a disease

---

## Limitations (Acknowledged by Authors)

1. **Database limitations**:
   - Potential miscoding (mitigated by SEER quality standards)
   - Missing clinicopathologic factors:
     - Patient comorbidities
     - Margin status (R0 vs R1 resection)
     - Details of adjuvant therapy

2. **Specimen heterogeneity**:
   - Cannot distinguish pylorus-preserving vs standard Whipple in SEER
   - Pylorus-preserving removes gastric LN (clinically less relevant but inflates count)
   - Authors limited to pancreatic head cancers to control for specimen type

3. **Surgical volume**:
   - SEER lacks hospital identifiers → cannot assess high- vs low-volume centers
   - Ko's group previously found minimal difference in colon cancer node yield by volume

4. **Survival analysis restricted to pN0**:
   - Intentionally excluded node-positive cohorts
   - Avoids confounding by putative therapeutic effect of lymphadenectomy extent
   - Cannot assess if extended LN dissection improves survival in pN1 disease

---

## Discussion Highlights

### Dr. Bilchik's Questions (Summary)

1. **High-volume centers**: SEER lacks identifiers; Ko's prior colon work showed little volume effect on node yield
2. **Tumor characteristics**: Differentiation remains significant after controlling for nodes; margin status not available in SEER
3. **Small bowel mesentery**: Minimal contribution; pylorus-preserving vs standard may differ
4. **Extended lymphadenectomy**: 15 nodes achievable with standard resection (Memorial, Hopkins examples)
5. **Surgery vs pathology**: **Both contribute** - surgeons remove, pathologists find
6. **Designated pancreatic pathologist**: UCLA has weekly multidisciplinary conferences with pathology feedback
7. **Stage migration vs micrometastasis**: Likely both, but stage migration definitely playing a role

### Dr. Byrd's Comment (Surgeon-Pathologist Collaboration)
- **Highest yield intervention**: Formal dialogue between surgeon and pathologist
- Use anatomic diagrams to show where nodes were removed
- Face-to-face discussion maintains credibility with medical oncology
- **Action item**: If all surgeons did this, node yield would dramatically improve

---

## Statistical Methods Summary

### Survival Analysis
- **Kaplan-Meier curves**: Non-parametric survival estimation
- **Log-rank test**: Comparison of survival curves (χ² maximization)
- **Cox proportional hazards regression**: Multivariate survival modeling
- **Hazard ratio interpretation**: HR<1 = protective, HR>1 = increased risk

### Lymph Node Cut Point (LNCP) Strategy
- **Systematic testing**: LNCPs from 2 to 25 nodes
- **Objective criterion**: Maximize log-rank χ² statistic
- **Validation**: Multivariate adjustment for confounders

### Software
- **Stata version 9** (Stata Corp, College Station, Texas)
- Significance threshold: P≤0.05

---

## Implementation in pathsampling Module

### Current Status (Post-Pu2021 Implementation)
The pathsampling module currently uses **Pu et al. 2021 (Johns Hopkins) recommendation of minELN=12** based on:
- Binomial probability calculation: n=11.6 rounded to 12
- Validation in n=1,837 pancreatic adenocarcinoma patients
- 95% confidence for adequate staging

### Tomlinson 2007 vs Pu 2021 Comparison

| Study Feature | Tomlinson 2007 (UCLA) | Pu 2021 (Johns Hopkins) |
|--------------|----------------------|------------------------|
| **Institution** | UCLA / SEER | Johns Hopkins |
| **Database** | SEER (population-based) | Single institution |
| **Sample size** | n=1,150 pN0 patients | n=1,837 total patients |
| **Study period** | 1988-2002 | Not specified (retrospective) |
| **Primary method** | Survival analysis (LNCP) | Binomial probability + survival |
| **Recommended minELN** | **15 nodes** | **12 nodes** |
| **Statistical basis** | Maximum χ²=14.49 at LNCP=15 | Binomial: P=1-(1-p)^n ≥0.95 |
| **Survival difference** | 8 months (19 vs 27 mo) | Validated LNR staging |
| **% achieving threshold** | 19% (in SEER data) | Not reported |
| **Additional findings** | Diminishing returns >15 nodes | LNR superior to AJCC N (12-28 ELN) |

### Reconciling the Discrepancy (15 vs 12)

**Possible Explanations**:
1. **Different statistical approaches**:
   - Tomlinson: Empirical survival maximization (log-rank χ²)
   - Pu: Theoretical binomial probability (95% confidence)

2. **Different populations**:
   - Tomlinson: SEER population-based (median=7 nodes, reflects community practice)
   - Pu: Academic center (higher baseline node yield)

3. **Different outcomes**:
   - Tomlinson: Overall survival in pN0 cohort
   - Pu: Detection probability + LNR classification + survival

4. **Era effects**:
   - Tomlinson: 1988-2002 data (older surgical/pathology practices)
   - Pu: More recent data (improved techniques)

5. **Clinical conservatism**:
   - Tomlinson: Proposes 15 as **quality measure** (higher bar)
   - Pu: Proposes 12 as **minimum adequacy** (floor)

**Synthesis**: Both studies validate that **12-15 nodes is the optimal range**. The pathsampling module should:
- Continue using **12 as minimum** (Pu 2021, widely accepted, matches colon standard)
- Add **15 as aspirational target** (Tomlinson 2007, quality measure)
- Allow user customization between 12-15 based on institutional goals

---

## Recommendations for pathsampling Module

### 1. Update Adequacy Assessment Thresholds

**Current Implementation** (Pu2021-based):
```
<9 ELN: Inadequate
9-11 ELN: Marginal
12-28 ELN: Adequate
≥29 ELN: Excellent
```

**Proposed Enhancement** (Incorporate Tomlinson2007):
```
<9 ELN: Inadequate (use N0 vs N+ only)
9-11 ELN: Marginal (possible understaging)
12-14 ELN: Adequate - Minimum (Pu2021 threshold)
15-28 ELN: Adequate - Quality Standard (Tomlinson2007 threshold)
≥29 ELN: Excellent (AJCC N marginally superior to LNR)
```

### 2. Add Tomlinson 2007 Reference to Documentation

**Current Text** (pathsampling.b.R lines 814-828):
```r
html <- "<h4>Lymph Node Ratio and Staging Analysis</h4>
<p>Based on Pu et al. (2021) from Johns Hopkins (n=1,837 pancreatic adenocarcinoma patients).</p>
...
```

**Proposed Enhancement**:
```r
html <- "<h4>Lymph Node Ratio and Staging Analysis</h4>
<p>Based on validated lymph node adequacy studies in pancreatic adenocarcinoma:</p>
<ul>
    <li><b>Pu et al. 2021 (Johns Hopkins, n=1,837):</b> minELN=12 for 95% adequacy (binomial model)</li>
    <li><b>Tomlinson et al. 2007 (UCLA/SEER, n=1,150):</b> minELN=15 for quality measure (survival analysis)</li>
</ul>
<p><b>Key Findings:</b></p>
<ul>
    <li>LNR classification optimized via X-tile analysis with survival endpoints (Pu 2021)</li>
    <li>Examination of ≥15 nodes associated with 8-month survival advantage (Tomlinson 2007)</li>
    <li>Consensus: 12-15 nodes optimal for accurate staging</li>
    <li>LNR superior to AJCC N staging when ELN 12-28</li>
</ul>
```

### 3. Update References Section

**Add to pathsampling.b.R lines 1154-1159**:
```r
<li><b>Lymph Node Adequacy & LNR Classification:</b>
    <ul>
        <li>Tomlinson JS, et al. Accuracy of Staging Node-Negative Pancreas Cancer.
            <i>Arch Surg.</i> 2007;142(8):767-774.
            <br><i>minELN=15 for quality measure, 8-month survival advantage (SEER n=1,150)</i></li>
        <li>Pu N, et al. An Artificial Neural Network Improves Prediction of Observed Survival
            in Patients with Pancreatic Cancer. <i>J Natl Compr Canc Netw.</i> 2021;19(9):1029-1036.
            <br><i>minELN=12 for 95% confidence, LNR thresholds 0.1 & 0.3 (n=1,837)</i></li>
    </ul>
</li>
```

### 4. Add Quality Measure Commentary

**New HTML section** (optional, for educational purposes):
```r
<p><b>Quality Measure Context:</b></p>
<ul>
    <li>National Quality Forum: 12 lymph nodes for colon cancer (accountability measure)</li>
    <li>Tomlinson 2007: Proposed 15 lymph nodes for pancreatic cancer quality measure</li>
    <li>Only 19% of SEER cases (1988-2002) achieved ≥15 nodes (median=7)</li>
    <li>Academic centers (Memorial, Hopkins, Fox Chase): median 15-17 nodes achievable</li>
</ul>
```

---

## Key Takeaways

### Clinical Practice
1. **Minimum adequacy**: 12 lymph nodes (Pu 2021, binomial probability)
2. **Quality measure**: 15 lymph nodes (Tomlinson 2007, survival maximization)
3. **Diminishing returns**: Marginal benefit beyond 15 nodes (3% per additional node)
4. **Detection sensitivity**: 90% of pN1a disease found with ≤15 nodes examined

### Staging Accuracy
- **Stage migration effect**: Inadequate sampling → understaging → worse survival
- **8-month survival difference**: pN0 with ≥15 vs <15 nodes (27 vs 19 months)
- **Independent predictor**: HR=0.63 (P<0.0001) after adjusting for age, sex, T stage

### Quality Improvement
- **Current performance**: Only 19% achieve ≥15 nodes (SEER 1988-2002)
- **Achievability**: Academic centers routinely achieve ≥15 nodes
- **Intervention**: Surgeon-pathologist collaboration (Dr. Byrd's recommendation)

### Research Implications
- **Trial design**: Ensure adequate nodal sampling to avoid maldistribution
- **Comparative studies**: Tomlinson (SEER, population) vs Pu (Hopkins, academic)
- **Future directions**: Micrometastasis detection, molecular staging

---

## Statistical Pearls

### Hazard Ratio Interpretation
- **HR = 0.63**: 37% reduction in mortality hazard
- **HR = 0.88**: 12% reduction per additional node
- **Predicted HR curve**: Demonstrates diminishing marginal benefit

### Log-Rank χ² Maximization
- Objective method for optimal cut-point selection
- Avoids arbitrary thresholds
- LNCP=15 had maximum χ²=14.49 (P=0.0001)

### pN1a Cohort as Validation
- Smallest nodal burden = most sensitive test
- 90% detection rule parallels diagnostic test development
- Confirms 15 nodes provide sufficient sampling density

---

## Concluding Statement

**Tomlinson et al. 2007 provides robust population-based evidence that examination of ≥15 lymph nodes after pancreaticoduodenectomy for adenocarcinoma:**

1. **Significantly improves survival** (8-month difference, P<0.001)
2. **Is an independent prognostic factor** (HR=0.63, P<0.0001)
3. **Provides diminishing returns beyond 15** (marginal 3% benefit per node)
4. **Detects 90% of minimal nodal disease** (pN1a cohort)
5. **Is realistic and achievable** (major centers routinely meet threshold)
6. **Should serve as quality measure** for pancreatic cancer surgery

**Combined with Pu 2021, the evidence supports a consensus range of 12-15 lymph nodes as optimal for accurate staging of pancreatic adenocarcinoma.**

---

*Document prepared for integration into ClinicoPath Jamovi Module pathsampling analysis*
*Date: 2025-10-10*
