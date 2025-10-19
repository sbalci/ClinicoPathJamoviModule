# Omentum Study Response Strategy

## Executive Summary

**Context**: Maglalang & Fadare (UCSD, 2025) published in *Am J Clin Pathol* claiming 1-2 blocks are sufficient for omentum sampling in gynecologic cancers (n=1,055), contradicting established guidelines (ISGyP/ICCR: ≥4 blocks).

**Critical Issues**: Retrospective design without gold standard, no false negative rate quantification, no clinical outcomes assessed, circular sampling bias.

**Our Response**: Prospective validation study with **complete sampling gold standard** to establish true sensitivity of limited-block protocols.

---

## Positioning Statement

### The Problem

International guidelines for omentum sampling in gynecologic cancer staging recommend ≥4 blocks (ISGyP 2019, ICCR 2022), but a recent large retrospective study (Maglalang & Fadare 2025, n=1,055) suggests 1-2 blocks may be sufficient. However, this conclusion is based on microscopic positivity rates (MPR) without a gold standard comparison, precluding assessment of false negative rates—the critical metric for staging adequacy.

### The Gap

No prospective study has compared limited sampling strategies to complete tissue sampling in omentectomy specimens. Without knowing how much disease is **missed** by 1-2 blocks, recommendations cannot be evidence-based.

### Our Contribution

We propose the first prospective validation study using **complete sampling as the gold standard** to:
1. Quantify false negative rates of 1-2 block sampling
2. Model detection probability using validated frameworks (binomial, hypergeometric)
3. Assess clinical impact on staging and survival outcomes
4. Provide risk-stratified recommendations (e.g., post-neoadjuvant cases require more sampling)

---

## Study Design Summary

**Design**: Prospective, two-phase sampling study

**Phase 1**: Complete Sampling Gold Standard (n=200)
- Prosector submits initial 2 blocks (standardized protocol)
- Study pathologist then **completely samples** all remaining tissue
- Compare: 2-block diagnosis vs complete sampling diagnosis
- Outcome: Sensitivity, false negative rate, predictors of missed disease

**Phase 2**: Probabilistic Modeling
- Binomial model: P(detection) = 1 - (1-p)^n
- Tissue volume-based adequacy (blocks per cm³)
- Clinical cutoff optimization (ROC analysis)

**Subgroups** (addressing Maglalang limitations):
- Gross appearance (normal, focal, multifocal, diffuse)
- Primary site (tubo-ovarian vs endometrial)
- Neoadjuvant chemotherapy (critical—Maglalang showed 5× higher MPR)
- Specimen characteristics (weight, dimensions)

**Clinical Outcomes** (5-year follow-up):
- Stage migration rate (understaging frequency)
- Adjuvant treatment decision changes
- Recurrence-free survival (RFS)
- Overall survival (OS)

---

## Methodological Innovations

### 1. Gold Standard Comparison
**Maglalang**: No gold standard → cannot assess false negatives
**Our Study**: Complete sampling → quantifies missed disease

### 2. Detection Probability Modeling
**Maglalang**: No modeling
**Our Study**: Binomial framework (validated in LN adequacy studies)
- Example: If disease prevalence = 10% per block, how many blocks for 95% detection?

### 3. Tissue Volume Accounting
**Maglalang**: Block-to-size ratio uses 1D measurement (widest diameter)
**Our Study**: Volume-based adequacy (blocks per gram or cm³)
- 6 blocks from 100g omentum ≠ 6 blocks from 500g omentum

### 4. Clinical Impact Assessment
**Maglalang**: No survival or staging outcomes
**Our Study**: Cox regression for survival, stage migration quantification

### 5. Risk Stratification
**Maglalang**: One-size-fits-all recommendation (1-2 blocks)
**Our Study**: Context-dependent thresholds:
- Normal, no NACT: 4 blocks (guideline)
- Normal, post-NACT: 6+ blocks (19% MPR per Maglalang)
- Multifocal/diffuse: 2-3 blocks (92% MPR)
- Focal abnormal: 4+ blocks (66% MPR, high variability)

---

## Pathsampling Module Implementation

### New Feature: Omentum Adequacy Assessment

**User Inputs**:
- Gross appearance (normal/focal/multifocal/diffuse)
- Blocks submitted
- Specimen dimensions & weight
- Primary tumor site
- Neoadjuvant status
- Histology

**Outputs**:
1. **Adequacy Classification**:
   - Inadequate (<2 blocks or high false-negative risk)
   - Marginal (2-3 blocks, intermediate risk)
   - Adequate (≥4 blocks, guideline-concordant)
   - Excellent (risk-stratified approach)

2. **Risk Assessment Table**:
   - Expected MPR (based on Maglalang data)
   - Estimated sensitivity (from our validation)
   - False negative probability
   - Block-to-size ratio
   - Block-to-volume ratio

3. **Clinical Recommendation**:
   ```
   Scenario: Normal omentum, post-NACT, high-grade serous ovarian ca, 2 blocks

   Assessment: ⚠️ INADEQUATE
   - Expected MPR: 19.0% (Maglalang post-NACT data)
   - Estimated Sensitivity: 75% (95% CI: 60-85%) [from our study]
   - False Negative Risk: 25%

   Recommendation: At least 6 blocks recommended

   Rationale:
   - Maglalang et al. 2025: 5× higher MPR in post-NACT cases (19% vs 4%)
   - Our validation data: 2 blocks have <90% sensitivity in this scenario
   - Clinical impact: Potential understaging from N0 to N1 affects adjuvant therapy

   References:
   - Maglalang NA, Fadare O. Am J Clin Pathol. 2025. doi:10.1093/ajcp/aqaf082
   - [Our study citation - pending]
   - ISGyP: Malpica A, et al. Int J Gynecol Pathol. 2019;38:S9-S24
   ```

4. **Comparison Visualization**:
   - Heatmap: Sensitivity by block count × gross appearance
   - Risk calculator: Interactive slider
   - Maglalang data vs our validation (side-by-side comparison)

---

## Key Messages for Publication

### Abstract (Structured)

**Background**: Maglalang & Fadare (2025) concluded 1-2 blocks are sufficient for omentum sampling in gynecologic cancers based on retrospective MPR analysis (n=1,055). However, without a gold standard, false negative rates remain unknown.

**Methods**: Prospective study with complete sampling gold standard (n=200 grossly normal omenta). Prosectors submitted initial 2 blocks; study pathologists completely sampled remaining tissue. Primary endpoint: sensitivity of 2-block sampling. Secondary: predictors of false negatives, clinical outcome correlation.

**Results**: [Pending]
- 2-block sensitivity: XX% (95% CI: XX-XX%)
- False negative rate: XX%
- Post-neoadjuvant cases: XX% false negative rate (vs XX% non-neoadjuvant, P=XX)
- Minimum blocks for 95% sensitivity: XX blocks
- Understaging rate: XX% with 2-block protocol vs XX% with complete sampling

**Conclusions**: [Pending, but likely:]
2-block sampling has insufficient sensitivity (< 90%) for detecting microscopic omental disease in high-risk scenarios. Evidence-based recommendations: ≥4 blocks standard, ≥6 blocks post-neoadjuvant. Prospective validation with gold standard comparison is essential before reducing sampling intensity.

---

### Introduction Framework

**Paragraph 1** - Clinical Importance:
> Omentectomy is a critical component of gynecologic cancer staging. Microscopic omental disease upstages 2-3% of "early-stage" ovarian cancers and 0.4-1.9% of endometrial cancers, directly impacting adjuvant treatment decisions and prognosis.

**Paragraph 2** - Current Guidelines:
> International guidelines (ISGyP 2019, ICCR 2022) recommend ≥4 blocks for grossly normal omenta, citing prior studies (Usubutun 2007, Skala & Hagemann 2015). However, these studies had methodological limitations (small n, simulation models), and optimal sampling remains debated.

**Paragraph 3** - Maglalang Study:
> Recently, Maglalang & Fadare (2025) analyzed 1,055 omentectomies and concluded 1-2 blocks are sufficient, as MPR did not statistically differ from higher sampling levels. This challenges established guidelines and could reduce pathology workload significantly.

**Paragraph 4** - Critical Limitation:
> However, retrospective MPR comparisons without a gold standard cannot assess false negative rates. If 2-block sampling misses 20% of microscopic disease, the study design cannot detect this discrepancy—cases with disease beyond sampled blocks appear "negative."

**Paragraph 5** - Our Contribution:
> We conducted the first prospective study comparing limited sampling to complete tissue sampling, establishing the true diagnostic accuracy of reduced protocols and their clinical impact on staging outcomes.

---

### Discussion Framework

**Paragraph 1** - Restate Main Finding:
> [Our sensitivity finding, e.g., "2-block sampling detected only 78% of microscopic disease..."]

**Paragraph 2** - Explain Discordance with Maglalang:
> Our findings differ from Maglalang et al.'s conclusion due to methodological design. Retrospective MPR analysis assumes that negative cases are true negatives, but our gold standard comparison revealed XX% false negatives—disease present on complete sampling but missed by initial 2 blocks.

**Paragraph 3** - Clinical Implications:
> Understaging due to inadequate sampling has direct clinical consequences. [Our data on stage migration, treatment changes, survival impact]

**Paragraph 4** - Post-Neoadjuvant Finding:
> Maglalang observed 5× higher MPR post-neoadjuvant (19% vs 4%), suggesting more extensive sampling needed. Our data confirm this: [our sensitivity in NACT vs non-NACT cases].

**Paragraph 5** - Risk-Stratified Approach:
> One-size-fits-all recommendations may be inappropriate. Our data support risk-stratified sampling: [table of recommendations by clinical scenario].

**Paragraph 6** - Methodological Lessons:
> This study demonstrates the importance of gold standard validation before practice-changing recommendations. Absence of evidence (no statistical difference in MPR) is not evidence of absence (adequate sensitivity).

**Paragraph 7** - Limitations & Future Directions:
> [Single-center, selection bias, need for multi-center validation, cost-effectiveness analysis]

---

## Timeline & Milestones

### Month 1-3: Study Setup
- IRB approval
- Protocol finalization
- Prosector training
- Data collection system setup

### Month 4-24: Enrollment
- Target: 200 grossly normal cases
- Target: 100 grossly abnormal cases (comparison)
- Complete sampling performed contemporaneously

### Month 25-30: Analysis & Manuscript
- Statistical analysis (sensitivity, false negative rates, predictors)
- Probabilistic modeling (binomial, tissue volume)
- Survival analysis (if follow-up available)
- Manuscript drafting
- Internal review

### Month 31-33: Submission & Revision
- Submit to *American Journal of Clinical Pathology* (same journal)
- Response to peer review
- Revision submission

### Month 34-36: Module Implementation
- Code omentum adequacy feature in pathsampling
- Integrate Maglalang data + our validation data
- User testing
- Documentation & vignettes
- Module release

---

## Budget Estimate

**Personnel**:
- Pathologist time: 50 hours @ $150/hr = $7,500
- Histotechnologist time: 15,000 blocks @ $10/block = $150,000
- Data management: 100 hours @ $50/hr = $5,000

**Materials**:
- Histology consumables (included in block cost)
- Photography equipment (existing)
- Database license (REDCap): $0 (institutional)

**Statistical**:
- R/jamovi (open-source): $0

**Total**: ~$162,500

**Funding Sources**:
- Departmental research funds
- Gynecologic oncology collaboration
- Pathology foundation grants
- Consider NIH R21 mechanism (exploratory study)

---

## Success Criteria

**Scientific**:
1. Establish sensitivity & false negative rate of 2-block sampling with narrow CI
2. Identify predictors of missed disease (multivariable model)
3. Quantify clinical impact (stage migration, survival)
4. Publish in high-impact pathology journal (*Am J Clin Pathol* or *Mod Pathol*)

**Clinical**:
1. Evidence-based sampling recommendations adopted in guidelines
2. Risk-stratification widely implemented
3. Pathsampling module used for institutional QA/QC

**Educational**:
1. Prosector training materials disseminated
2. Webinar/workshop presentations
3. Module tutorials and vignettes

---

## Key Takeaways

1. **Maglalang study is provocative but methodologically limited** (no gold standard)
2. **Our response provides definitive validation** (complete sampling)
3. **Pathsampling module integrates both datasets** (Maglalang + ours) for real-time adequacy assessment
4. **Risk-stratified approach** better than one-size-fits-all
5. **Post-neoadjuvant cases require special attention** (19% MPR)
6. **This exemplifies rigorous response to practice-changing claims** (don't change practice based on retrospective data alone)

---

## Next Steps

1. ✅ Complete comprehensive analysis of Maglalang study (DONE)
2. ⏳ Draft study protocol (IRB submission)
3. ⏳ Implement omentum adequacy feature in pathsampling module
4. ⏳ Create test data for module validation
5. ⏳ Compile and document module updates

---

## References

**Study Being Critiqued**:
- Maglalang NA, Fadare O. Pathologic sampling of the omentum for neoplasms that involve the female genital tract: A retrospective analysis of 1055 cases. *Am J Clin Pathol.* 2025. doi:10.1093/ajcp/aqaf082

**Supporting Our Methodology**:
- Skala SL, Hagemann IS. Optimal sampling of grossly normal omentum in staging of gynecologic malignancies. *Int J Gynecol Pathol.* 2015;34:281-287. [Simulation model, complete sampling conceptual framework]
- Tomlinson JS, et al. Accuracy of staging node-negative pancreas cancer. *Arch Surg.* 2007;142(8):767-774. [Survival-based adequacy threshold validation]
- Pu N, et al. An artificial neural network improves prediction of observed survival in patients with pancreatic Cancer. *J Natl Compr Canc Netw.* 2021;19(9):1029-1036. [Binomial detection modeling for LN adequacy]
- Yoon SJ, et al. Optimal number of lymph nodes retrieved to lower false N0 risk in pancreatic cancer. *Ann Surg Oncol.* 2025. doi:10.1245/s10434-025-18029-7 [False negative modeling, validation cohorts]

**Guidelines Being Defended**:
- Malpica A, et al. Endometrial carcinoma, grossing and processing issues: recommendations of the International Society of Gynecologic Pathologists. *Int J Gynecol Pathol.* 2019;38:S9-S24.
- Matias-Guiu X, et al. Data set for the reporting of endometrial cancer: recommendations from the International Collaboration on Cancer Reporting (ICCR). *Int J Gynecol Pathol.* 2022;41:S90-S118.
