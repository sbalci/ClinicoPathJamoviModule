# Citation Review: AI-Assisted Interpretation of Ki-67 Expression and Repeatability in Breast Cancer

---

## ARTICLE SUMMARY

- **Title/Label**: Artificial intelligence-assisted interpretation of Ki-67 expression and repeatability in breast cancer
- **Design & Cohort**: Prospective assessment study; N = 300 invasive breast cancer cases (all female, no neoadjuvant therapy) from the Fourth Hospital of Hebei Medical University (Oct 2017 - Oct 2019); split into training set (n=150) and validation set (n=150). Ki-67 IHC evaluated using four methods: visual assessment (VA), microscopic manual counting (MC), standard reference card (SRC), and AI (Inception V3 + ResNet deep learning). Nine pathologists (3 junior, 3 intermediate, 3 senior) for training set; 3 pathologists (1 per experience level) for validation set. Gold standard: combined AI + manual counting by 3 breast pathologists across 9 regions per slide.
- **Key Analyses**:
  - Intraclass correlation coefficient (ICC) for consistency between methods and gold standard
  - Kolmogorov-Smirnov test for normality assessment
  - Median and quartile spacing (M ± Q) for non-normal data description
  - Bland-Altman scatterplot for consistency visualization
  - Heat map visualization of Ki-67 interpretation patterns
  - Stratified analysis by tumor heterogeneity (homogeneous vs heterogeneous)

---

## ARTICLE CITATION

| Field | Value |
|-------|-------|
| Title | Artificial intelligence-assisted interpretation of Ki-67 expression and repeatability in breast cancer |
| Journal | Diagnostic Pathology |
| Year | 2022 |
| Volume | 17 |
| Issue | 1 |
| Pages | 20 (1-10) |
| DOI | 10.1186/s13000-022-01196-6 |
| PMID | TODO |
| Publisher | BioMed Central / Springer Nature |
| ISSN | 1746-1596 |
| First Author | Lina Li |
| Corresponding Author | Yueping Liu (annama@163.com) |
| Received / Accepted | April 15, 2021 / January 18, 2022 |
| License | CC BY 4.0 |

---

## Skipped Sources

*None -- PDF was read successfully (10 pages).*

---

## EXTRACTED STATISTICAL METHODS

| Method / Model | Role (primary/secondary) | Variants & Options | Assumptions/Diagnostics | References (sec/page) |
|---|---|---|---|---|
| Kolmogorov-Smirnov test | Preliminary -- normality assessment | Applied to Ki-67LI in each group (VA, MC, SRC, AI); all groups non-normal (p < 0.05) | Justifies use of nonparametric methods and median reporting | Statistical Analysis (p5) |
| Median and quartile spacing (M ± Q) | Descriptive -- central tendency and dispersion | Used instead of mean ± SD due to non-normal distributions | Appropriate for skewed data | Statistical Analysis (p5) |
| Intraclass Correlation Coefficient (ICC) | Primary -- inter-rater and inter-method consistency | Single ICC values with 95% CIs; interpreted using Fleiss reference: <0.4 poor, 0.4-0.69 normal, 0.7-0.79 better, ≥0.8 very good | Not specified if ICC(1,1), ICC(2,1), or ICC(3,1); likely ICC(2,1) for consistency | Statistical Analysis (p5), Tables 2-4 |
| Bland-Altman scatterplot | Secondary -- consistency visualization | Used to check agreement between methods; not quantitatively analyzed (no LoA reported) | Paired measurements; visual assessment only | Statistical Analysis (p5) |
| Heat map | Secondary -- visualization | 9 pathologists × 150 cases × 4 methods + gold standard; qualitative comparison of interpretation patterns | Visual tool; no formal statistical test applied to heat maps | Results (p7), Fig 6 |
| Stratified analysis by heterogeneity | Secondary -- subgroup analysis | Homogeneous (121 cases, 40.33%) vs heterogeneous (179 cases, 59.67%) tumors; ICCs reported separately for each group | Stratification based on visual assessment of tumor Ki-67 distribution | Results (p5-6, p9), Table 4 |
| Time comparison | Descriptive | VA: 10-40s, MC: 240-480s, SRC: 8-30s, AI: 100-120s per slide | Not formally tested; descriptive comparison only | Table 1 (p6) |

---

## CLINICOPATH JAMOVI COVERAGE MATRIX

| Article Method | Jamovi Function(s) | Coverage | Notes / Workarounds |
|---|---|:---:|---|
| Intraclass Correlation Coefficient (ICC) | `agreement` (icc option, all 6 types), `icccoeff` | ✅ | `agreement` provides ICC(1,1) through ICC(3,k) with 95% CIs; `icccoeff` is dedicated ICC function |
| Bland-Altman analysis | `agreement` (blandAltmanPlot), `pathologyagreement` | ✅ | Full Bland-Altman with LoA, mean difference; more thorough than the article's visual-only use |
| Kolmogorov-Smirnov normality test | base R / descriptive statistics | ✅ | Available in descriptive statistics modules |
| Median with IQR | `tableone`, descriptive statistics | ✅ | Standard descriptive output |
| Heat map visualization | `jjheatmap` (ggstatsplot) | 🟡 | `jjheatmap` exists but designed for correlation matrices, not rater × case agreement heat maps; would need custom data preparation |
| Stratified ICC analysis | `agreement` with data subsetting | ✅ | Users can subset data by heterogeneity status and run ICC separately |
| Inter-method agreement (4 methods) | `agreement` (multiple raters), `pathologyagreement` | ✅ | `agreement` handles 2+ raters; Fleiss' kappa for categorical, ICC for continuous |
| Standard reference card calibration workflow | — | ❌ | No equivalent; SRC is a visual calibration tool, not a statistical analysis; outside scope of statistical software |
| AI-assisted Ki-67 counting | — | ❌ | Image analysis step (Inception V3 + ResNet); outside scope of statistical analysis module |

---

## CRITICAL EVALUATION

### Statistical Rigor Score: 🟡 Moderate (9/18)

| Criterion | Score | Notes |
|---|---|---|
| Sample size justification | 0/2 | No power analysis or sample size rationale; 300 cases chosen without justification |
| Multiple testing correction | 0/2 | Multiple ICCs compared across 9-12 pathologists × 4 methods without correction; all reported as P < 0.001 |
| Effect sizes with CIs | 2/2 | All ICCs reported with 95% CIs; consistent throughout Tables 2-4 |
| Assumption checking | 1/2 | Normality tested (KS); nonparametric descriptives used; but ICC form (1,1 vs 2,1 vs 3,1) not specified |
| Appropriate test selection | 1/2 | ICC is appropriate for continuous agreement; but Bland-Altman used only visually (no quantitative LoA); the ICC interpretation thresholds cite kappa guidelines (Ref 14), which are not directly applicable to ICC |
| Reproducibility | 2/2 | 9 pathologists with varied experience; two separate reading sessions 2 weeks apart (within-rater); training + validation split |
| Handling of missing data | 1/2 | Not discussed; appears complete for included cases |
| Model diagnostics | 0/2 | No ICC model specification; no residual diagnostics; no assessment of ICC assumptions (no random effects model) |
| Clinical significance assessment | 1/1 | ICC thresholds mapped to clinical interpretation (very good = clinically acceptable); time comparisons provided |
| External validity | 0/1 | Single institution; single AI platform; no external validation |
| Bias assessment | 1/1 | Region selection bias acknowledged in Discussion; gold standard combines AI + human to reduce bias |

### Strengths
1. **Large multi-rater design**: 9 pathologists × 150 cases with 4 methods each -- comprehensive assessment
2. **Experience stratification**: Junior, intermediate, and senior pathologists tested separately -- reveals experience-dependent effects
3. **Heterogeneity stratification**: Separate analysis for homogeneous vs heterogeneous tumors -- clinically meaningful subgroups
4. **Combined gold standard**: AI + manual counting by 3 specialists across 9 regions per slide -- robust reference
5. **Time comparison**: Practical workflow data comparing VA (10-40s), SRC (8-30s), MC (240-480s), AI (100-120s) per slide
6. **Two-week washout**: Pathologists re-scored after 2 weeks, reducing memory bias for within-rater consistency

### Weaknesses
1. **ICC model not specified**: Critical omission -- ICC(1,1), ICC(2,1), and ICC(3,1) have different interpretations; readers cannot judge which model was used
2. **No Bland-Altman quantification**: Scatterplots shown but no limits of agreement, mean difference, or regression line reported
3. **Kappa interpretation thresholds applied to ICC**: The cutoffs cited (Ref 14, Fleiss 1973) are for weighted kappa, not ICC; this is a common but incorrect practice
4. **No formal method comparison**: No direct statistical test comparing SRC-ICC vs AI-ICC vs VA-ICC (e.g., overlapping CIs or formal test of ICC difference)
5. **Single institution**: All cases from one hospital; no external validation
6. **No Ki-67 cutpoint analysis**: No analysis of agreement at the clinically relevant 14% or 20% thresholds -- categorical reclassification not assessed
7. **Heterogeneity classification subjective**: Based on visual assessment by 2 senior pathologists; no formal criteria
8. **AI software not publicly identified**: "Artificial intelligence software" based on Inception V3 + ResNet described only generally

---

## GAP ANALYSIS

### Gap 1: Formal ICC Comparison Between Methods

**What the article needed**: The article reports ICC values for 4 methods (VA, MC, SRC, AI) against the gold standard but does not formally test whether the ICC of one method differs significantly from another (e.g., Is ICC_SRC = 0.918 significantly different from ICC_VA = 0.757?).

**Current ClinicoPath coverage**: `agreement` and `icccoeff` report ICC values with 95% CIs. Users can visually compare overlapping CIs, but no formal test for ICC difference is provided.

**Gap**: A formal test for comparing two ICCs, analogous to testing whether two correlation coefficients differ. This could use:
- Fisher's z-transformation approach for comparing correlated ICCs
- Bootstrap test for ICC difference
- Zou's (2007) confidence interval for ICC difference

**Priority**: Low-Medium -- overlapping CIs provide reasonable informal comparison; formal tests are rarely used in the pathology literature.

### Gap 2: Heat Map for Rater × Case Agreement Visualization

**What the article used**: Heat map showing 9 pathologists' Ki-67LI values for 150 cases across 4 methods + gold standard (Fig 6). This visual immediately reveals rater disagreement patterns.

**Current ClinicoPath coverage**: `jjheatmap` exists but is designed for correlation matrices, not rater × case agreement data. No dedicated agreement heat map function.

**Gap**: A rater × case heat map visualization where rows = cases, columns = raters, color = Ki-67% (or any continuous score). This would complement the existing ICC/Bland-Altman analysis in `agreement`.

**Priority**: Low -- this is primarily a visualization enhancement; the statistical analysis is already fully covered.

---

## IMPLEMENTATION ROADMAP

### Not Recommended: Gap 1 (ICC comparison test)

ICC comparison tests are rarely needed in clinical practice. The overlapping CI approach is sufficient for most purposes, and the formal tests (Zou 2007) are complex to implement with limited demand. Users needing formal ICC comparisons can use R directly.

### Not Recommended: Gap 2 (Agreement heat map)

While visually informative, the rater × case heat map is a niche visualization that can be created in R with `ggplot2::geom_tile()`. Adding it to the jamovi module would add complexity with limited demand.

---

## OVERALL ASSESSMENT

| Dimension | Rating |
|---|---|
| Statistical rigor | 🟡 Moderate (9/18) |
| Methodological novelty | Low (standard ICC-based agreement study) |
| Clinical relevance | High (Ki-67 SRC as practical calibration tool; AI validation for clinical use) |
| Coverage by ClinicoPath | Excellent (7 covered, 1 partial, 2 gaps -- both outside statistical scope) |
| Implementation priority | None -- no gaps worth implementing |

### Key Takeaway for ClinicoPath Development

This article uses basic agreement statistics (ICC, Bland-Altman) that are **fully covered** by the existing `agreement`, `icccoeff`, and `pathologyagreement` functions. The two identified gaps (ICC comparison test, agreement heat map) are minor enhancements that would serve niche use cases.

The article does highlight the importance of **specifying the ICC model** (1,1 vs 2,1 vs 3,1) -- a common omission in pathology studies. The `agreement` function in ClinicoPath correctly distinguishes all 6 ICC forms, which is a strength compared to the article's ambiguous reporting.

The main clinical contribution is demonstrating that SRC + AI achieves ICC > 0.90 with the gold standard across experience levels, supporting standardized Ki-67 assessment workflows. This validates the clinical utility of the agreement analysis tools already in ClinicoPath.
