# Citation Review: Intra- and Inter-Observer Variability in Manual HER2 Scoring: Glass Slides vs Digital Images

---

## ARTICLE SUMMARY

- **Title/Label**: Comparative study of intra- and inter-observer variability in manual scoring of HER2 immunohistochemical stains on glass slides versus paired digital images with emphasis on the low end of the expression spectrum
- **Design & Cohort**: Retrospective concordance study; N = 247 breast carcinomas (117 core biopsies + 130 excisions, including 12 metastatic) from UCSF CoPath laboratory (2021-2024). Oversampled HER2 score 0 (n=100). Three experienced breast pathologists independently scored all glass slides and paired digital images (Philips WSI, 20x). 5-category scoring: null (no staining), ultralow (faint ≤10%), 1+, 2+, 3+. Washout period ≥2 weeks between slide and image reads.
- **Key Analyses**:
  - Cohen's kappa (weighted) for intra-observer agreement (glass slide vs digital image per pathologist)
  - Cohen's kappa for inter-observer agreement (between pathologists, all 5 categories and lowest 3 categories)
  - Confusion matrices (individual and consensus scores)
  - Concordance of research scores with clinical pathology reports
  - Re-classification analysis of clinical HER2 0 into null vs ultralow subcategories

---

## ARTICLE CITATION

| Field | Value |
|-------|-------|
| Title | Comparative study of intra- and inter-observer variability in manual scoring of HER2 immunohistochemical stains on glass slides versus paired digital images with emphasis on the low end of the expression spectrum |
| Journal | Human Pathology |
| Year | 2025 |
| Volume | 161 |
| Pages | 105860 |
| DOI | 10.1016/j.humpath.2025.105860 |
| PMID | TODO |
| Publisher | Elsevier |
| First Author | Andrew Xiao |
| Corresponding Author | Joseph Geradts (joseph.geradts@duke.edu) |
| Received / Accepted | April 29, 2025 / June 24, 2025 |
| License | All rights reserved (Elsevier) |

---

## Skipped Sources

*None -- PDF was read successfully (6 pages).*

---

## EXTRACTED STATISTICAL METHODS

| Method / Model | Role (primary/secondary) | Variants & Options | Assumptions/Diagnostics | References (sec/page) |
|---|---|---|---|---|
| Cohen's kappa (unweighted) | Primary -- intra-observer and inter-observer agreement | Kappa with 95% CI; all 5 categories and lowest 3 categories (null/ultralow/1+) analyzed separately | Nominal scale; p < 0.05 significance threshold | Methods 2.2 (p2), Tables 2-4 |
| Descriptive statistics | Secondary -- frequencies and percentages | Categorical variable summaries | None | Methods 2.2 (p2) |
| Confusion/concordance matrices | Primary -- cross-tabulation of scores | Individual pairs (n=741) and consensus pairs (n=247); concordant scores highlighted in bold | No formal test of symmetry or marginal homogeneity | Tables 1, 5 |
| Consensus scoring | Secondary -- majority rule | 2/3 or 3/3 agreement defines consensus score; used for both glass slide and digital image scoring | Assumes independent assessments | Methods 2.1 (p2) |

---

## CLINICOPATH JAMOVI COVERAGE MATRIX

| Article Method | Jamovi Function(s) | Coverage | Notes / Workarounds |
|---|---|:---:|---|
| Cohen's kappa (2 raters) | `agreement` (kappa2), `pathologyagreement` | ✅ | Standard Cohen's kappa with 95% CI |
| Cohen's kappa (multi-category, 5 levels) | `agreement` (kappa2), `pathologyagreement` | ✅ | Works with any number of categories |
| Inter-observer agreement (3 raters) | `agreement` (kappam.fleiss), `pathologyagreement` | ✅ | Fleiss' kappa for 3+ raters |
| Confusion/concordance matrix | `agreement` (multiClassConfusionMatrix) | ✅ | Multi-class confusion matrix implemented 2026-02-08 |
| Concordance rate (percent agreement) | `agreement`, `pathologyagreement` | ✅ | Overall percent agreement reported |
| Subgroup kappa (lowest 3 categories only) | `agreement` with data subsetting | ✅ | Users can filter to null/ultralow/1+ categories |
| Consensus determination (majority rule) | — | ❌ | Not a statistical analysis; data preparation step |
| HER2 ultralow sub-classification | — | ❌ | Clinical classification scheme, not statistical method |

---

## CRITICAL EVALUATION

### Statistical Rigor Score: 🟡 Moderate (10/18)

| Criterion | Score | Notes |
|---|---|---|
| Sample size justification | 1/2 | Oversampled HER2 0 (n=100) by design to study low end; no formal power analysis |
| Multiple testing correction | 0/2 | 12+ kappa values reported (3 pathologists × 2 category schemes × 2 modalities) without correction |
| Effect sizes with CIs | 2/2 | All kappa values with 95% CIs consistently reported |
| Assumption checking | 1/2 | Appropriate use of unweighted kappa for ordinal categories; no discussion of linearly vs quadratically weighted alternatives |
| Appropriate test selection | 2/2 | Cohen's kappa appropriate for paired rater agreement; separate analyses for 5 vs 3 categories is methodologically sound |
| Reproducibility | 2/2 | 3 experienced pathologists; ≥2 week washout; paired glass/digital for same cases; blinded to prior reads |
| Handling of missing data | 1/2 | 1 case without clinical HER2 score excluded; otherwise complete |
| Model diagnostics | 0/2 | No prevalence-adjusted kappa; no discussion of kappa paradoxes (high agreement with low kappa due to prevalence imbalance) |
| Clinical significance assessment | 1/1 | Re-classification implications for ADC therapy eligibility discussed; 7% upgraded from 0 to 1+ |
| External validity | 0/1 | Single institution (UCSF); experienced breast pathologists only |
| Bias assessment | 0/1 | Selection bias from oversampled HER2 0; acknowledged but not analyzed for impact on kappa values |

### Strengths
1. **Novel comparison**: First study (per authors) to compare glass slide vs digital image HER2 scoring with paired images
2. **5-category scoring with ultralow**: Clinically relevant sub-stratification of HER2 0 (null vs ultralow) per emerging guidelines
3. **Separate low-end analysis**: Dedicated analysis of the most clinically challenging categories (null/ultralow/1+)
4. **Intra-observer consistency**: Same pathologist scoring both glass slides and paired digital images with washout
5. **Clinical score concordance**: Comparison with original pathology reports reveals real-world re-classification rates
6. **High-quality kappa values**: Inter-observer kappa 0.82-0.87 (near-perfect) across all 5 categories

### Weaknesses
1. **No weighted kappa**: Ordinal scale (null < ultralow < 1+ < 2+ < 3+) should use quadratic weighted kappa; unweighted kappa treats all disagreements equally
2. **No prevalence-adjusted kappa**: Oversampled HER2 0 creates prevalence imbalance; PABAK would be informative
3. **Small number of raters**: Only 3 pathologists; Fleiss' kappa would better characterize multi-rater agreement
4. **No formal test of modality difference**: Glass vs digital kappa values compared visually but not formally tested (e.g., no test for kappa₁ ≠ kappa₂)
5. **Single institution with experienced raters**: High agreement may not generalize to community pathology settings

---

## GAP ANALYSIS

### Gap 1: Weighted Kappa Options

**What the article should have used**: Quadratic weighted kappa for ordinal HER2 scoring (null < ultralow < 1+ < 2+ < 3+), which penalizes larger disagreements more heavily than adjacent-category disagreements.

**Current ClinicoPath coverage**: `agreement` already supports weighted kappa through `irr::kappa2(weight="squared")`.

**Status**: ✅ Already covered -- no gap.

### Gap 2: Prevalence-Adjusted Bias-Adjusted Kappa (PABAK)

**What would improve the analysis**: PABAK adjusts for prevalence imbalance that artificially depresses kappa values; relevant when some categories are oversampled (as here with HER2 0).

**Current ClinicoPath coverage**: Not explicitly available as a named option.

**Gap**: Minor -- PABAK is a simple correction: PABAK = 2 × P_o - 1, where P_o is observed proportion of agreement. Could be added as an option to the `agreement` function.

**Priority**: Low -- PABAK is trivially calculated from existing agreement output.

---

## IMPLEMENTATION ROADMAP

### Not Recommended: All Gaps

Both identified gaps are either already covered (weighted kappa) or trivially derivable from existing output (PABAK). No implementation changes needed.

---

## OVERALL ASSESSMENT

| Dimension | Rating |
|---|---|
| Statistical rigor | 🟡 Moderate (10/18) |
| Methodological novelty | Medium (first paired glass/digital HER2 comparison; ultralow subcategorization) |
| Clinical relevance | Very High (HER2-low/ultralow distinction for ADC therapy eligibility) |
| Coverage by ClinicoPath | Excellent (6 covered, 0 partial, 2 outside-scope) |
| Implementation priority | None -- all statistical methods fully covered |

### Key Takeaway for ClinicoPath Development

This article uses standard agreement statistics (Cohen's kappa, confusion matrices) that are **fully covered** by the existing `agreement` and `pathologyagreement` functions. The clinical importance lies in the HER2 ultralow subcategorization and glass-vs-digital concordance, which are analytical questions rather than statistical method gaps.

The article actually demonstrates a common methodological weakness (using unweighted kappa for ordinal data) that ClinicoPath already handles correctly by offering weighted kappa options. This reinforces the value of ClinicoPath's comprehensive agreement analysis toolkit.
