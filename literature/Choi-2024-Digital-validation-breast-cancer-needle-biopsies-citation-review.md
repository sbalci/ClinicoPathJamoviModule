# Citation Review: Digital Validation in Breast Cancer Needle Biopsies -- CLM vs WSI vs DIA

---

## ARTICLE SUMMARY

- **Title/Label**: Digital Validation in Breast Cancer Needle Biopsies: Comparison of Histological Grade and Biomarker Expression Assessment Using Conventional Light Microscopy, Whole Slide Imaging, and Digital Image Analysis
- **Design & Cohort**: Retrospective validation study; N = 101 primary breast cancer cases (115 specimens, 14 excluded for scant cells/poor fixation) from US-guided core needle biopsy at Chungnam National University Sejong Hospital (July 2020 - Dec 2022). Three board-certified pathologists with varying experience (3-25 years). Three modalities compared: CLM (glass slides), WSI (PANNORAMIC 250 Flash III, 40×), and DIA (QuantCenter 2.2, 3DHISTECH). Assessed: Nottingham grade (TF, NP, MCs), ER, PR, HER2, Ki67. ≥3 month washout between CLM and WSI reads.
- **Key Analyses**:
  - Cohen's kappa for intra-observer agreement (CLM vs WSI per pathologist)
  - Fleiss' kappa with 95% CIs for inter-observer agreement (3 pathologists)
  - Wilcoxon signed-rank test for paired CLM/WSI differences
  - Kolmogorov-Smirnov test for normality
  - Concordance rates (perfect/minor/major discordance)
  - DIA validation: kappa between DIA and CLM, DIA and WSI, DIA and consensus

---

## ARTICLE CITATION

| Field | Value |
|-------|-------|
| Title | Digital Validation in Breast Cancer Needle Biopsies: Comparison of Histological Grade and Biomarker Expression Assessment Using Conventional Light Microscopy, Whole Slide Imaging, and Digital Image Analysis |
| Journal | Journal of Personalized Medicine |
| Year | 2024 |
| Volume | 14 |
| Issue | 3 |
| Pages | 312 |
| DOI | 10.3390/jpm14030312 |
| PMID | TODO |
| Publisher | MDPI |
| First Author | Ji Eun Choi |
| Corresponding Author | Ji Eun Choi & Dong-Wook Kang |
| Received / Accepted | Feb 6, 2024 / March 14, 2024 |
| License | CC BY 4.0 |

---

## Skipped Sources

*None -- PDF was read successfully (16 pages).*

---

## EXTRACTED STATISTICAL METHODS

| Method / Model | Role (primary/secondary) | Variants & Options | Assumptions/Diagnostics | References (sec/page) |
|---|---|---|---|---|
| Cohen's kappa | Primary -- intra-observer agreement (CLM vs WSI) | Per pathologist; kappa for NG and each component (TF, NP, MCs); kappa for biomarker expression (ER, PR, HER2, Ki67) | Nominal/ordinal agreement measure; Landis & Koch (1977) interpretation scale | Methods 2.6 (p5), Figs 4-5, Tables S1-S2 |
| Fleiss' kappa with 95% CI | Primary -- inter-observer agreement (3 pathologists) | Separate for CLM and WSI; for NG components and biomarkers | Extension of kappa to 3+ raters; all p < 0.001 | Methods 2.6 (p5), Tables 3, 6 |
| Kolmogorov-Smirnov test | Preliminary -- normality assessment | Differences in scores not normally distributed (p < 0.01) | Justifies nonparametric tests | Methods 2.6 (p5) |
| Wilcoxon signed-rank test | Secondary -- paired CLM/WSI comparison | Per observer for NG components and biomarker expression | Nonparametric paired test for nonnormal differences | Methods 2.6 (p5), Tables 2, 5 |
| Concordance classification | Descriptive -- agreement categorization | Perfect concordance (exact match), minor discordance (1 level), major discordance (>1 level or clinical significance change) | Clinical criteria for discordance defined separately for grade and biomarkers | Methods 2.5 (p4-5), Tables 1, 4 |
| Intra-class agreement (DIA comparison) | Secondary -- method validation | Kappa between DIA and CLM, DIA and WSI, DIA and consensus for each biomarker | Treats DIA as a "rater" | Results 3.6 (p9-10), Figs 6-7, Table S3-S4 |

---

## CLINICOPATH JAMOVI COVERAGE MATRIX

| Article Method | Jamovi Function(s) | Coverage | Notes / Workarounds |
|---|---|:---:|---|
| Cohen's kappa (intra-observer) | `agreement` (kappa2), `pathologyagreement` | ✅ | Standard Cohen's kappa with 95% CI |
| Fleiss' kappa (inter-observer, 3 raters) | `agreement` (kappam.fleiss), `pathologyagreement` | ✅ | Fleiss' kappa with 95% CIs for 3+ raters |
| Wilcoxon signed-rank test | `jjhistostats` (ggwithinstats), base jamovi | ✅ | Available for paired nonparametric comparisons |
| Kolmogorov-Smirnov test | base R / descriptive statistics | ✅ | Standard normality test |
| Concordance rate (perfect/minor/major) | `agreement`, `pathologyagreement` | ✅ | Percent agreement; major/minor discordance requires custom definition |
| Kappa per biomarker (ER, PR, HER2, Ki67) | `agreement` (run separately per biomarker) | ✅ | Users run agreement analysis for each biomarker pair |
| DIA vs pathologist agreement | `agreement`, `pathologyagreement` | ✅ | Treat DIA as additional "rater" |
| Nottingham grade component analysis | `agreement` (run per component) | ✅ | TF, NP, MCs scored as ordinal; kappa per component |

---

## CRITICAL EVALUATION

### Statistical Rigor Score: 🟡 Moderate (10/18)

| Criterion | Score | Notes |
|---|---|---|
| Sample size justification | 0/2 | No power analysis; n=101 is small for multi-category agreement |
| Multiple testing correction | 0/2 | 24+ kappa values (3 observers × 8 measures × CLM/WSI) without correction |
| Effect sizes with CIs | 2/2 | Fleiss' kappa with 95% CIs for inter-observer; all p values reported |
| Assumption checking | 2/2 | KS normality test; nonparametric tests throughout; appropriate methods |
| Appropriate test selection | 2/2 | Cohen's kappa for intra-observer, Fleiss' kappa for inter-observer, Wilcoxon for paired differences -- all appropriate |
| Reproducibility | 2/2 | 3 pathologists; ≥3 month washout; all modalities on same cases; DIA automated |
| Handling of missing data | 1/2 | 14 of 115 excluded (scant cells/poor fixation); remaining appear complete |
| Model diagnostics | 0/2 | No weighted kappa for ordinal data; no prevalence adjustment |
| Clinical significance assessment | 1/1 | Major/minor discordance defined clinically; DIA improvement highlighted |
| External validity | 0/1 | Single institution; all CNB specimens |
| Bias assessment | 0/1 | Experience-dependent effects not formally tested (visual comparison only) |

### Strengths
1. **Three-way comparison**: CLM vs WSI vs DIA -- comprehensive method validation
2. **CNB focus**: Clinically important since CNB specimens have unique challenges (small tissue, heterogeneity)
3. **Multiple biomarkers**: ER, PR, HER2, Ki67 all assessed alongside Nottingham grade
4. **Component-level analysis**: Separate analysis for TF, NP, and MCs identifies which component drives grade discordance (NP most variable)
5. **DIA validation**: Shows DIA can improve kappa vs inter-observer agreement for biomarker expression
6. **High-magnification scanning**: 40× WSI (0.121 μm/pixel) -- addresses previous concerns about mitotic counting at lower magnification

### Weaknesses
1. **Small sample size**: n=101 limits power and precision of kappa estimates
2. **No weighted kappa**: Ordinal Nottingham grade and components should use weighted kappa
3. **NP subjectivity**: Nuclear pleomorphism had lowest agreement (kappa ≤ 0.394 inter-observer) -- known limitation of current grading system
4. **Single scanner**: PANNORAMIC 250 Flash III only; scanner-specific effects not assessed
5. **No formal test of CLM=WSI**: Kappa values compared visually without formal hypothesis test
6. **DIA software-specific**: QuantCenter 2.2 results may not generalize to other DIA platforms

---

## GAP ANALYSIS

No significant gaps identified. All statistical methods used (Cohen's kappa, Fleiss' kappa, Wilcoxon, concordance rates) are fully covered by existing ClinicoPath functions (`agreement`, `pathologyagreement`, `jjhistostats`).

The article's findings reinforce that NP scoring is the weakest link in Nottingham grading (kappa ≤ 0.394), which is a clinical problem rather than a statistical method gap.

---

## IMPLEMENTATION ROADMAP

### Not Recommended: No gaps to implement

All statistical methods are fully covered.

---

## OVERALL ASSESSMENT

| Dimension | Rating |
|---|---|
| Statistical rigor | 🟡 Moderate (10/18) |
| Methodological novelty | Low-Medium (standard CLM/WSI/DIA comparison; CNB focus is somewhat novel) |
| Clinical relevance | High (validation of WSI and DIA for clinical-grade biomarker assessment on CNB) |
| Coverage by ClinicoPath | Excellent (8 covered, 0 partial, 0 gaps) |
| Implementation priority | None -- complete coverage |

### Key Takeaway for ClinicoPath Development

This article provides a clean validation use case for the `agreement` and `pathologyagreement` functions. All statistical methods used are fully covered. The article's finding that DIA improves inter-observer agreement for biomarker expression supports the clinical value of standardized digital pathology workflows that ClinicoPath's agreement analysis tools are designed to evaluate.
